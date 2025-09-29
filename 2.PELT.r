library(data.table)
library(changepoint)  # PELT/MBIC
library(zoo)
data_path <- "/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/multi_log_return_data.csv"
multi_data <- fread(data_path)
print(head(multi_data))

md <- copy(multi_data)
if ("V1" %in% names(md)) md[, V1 := NULL]
if ("Date" %in% names(md) && !"date" %in% names(md)) setnames(md, "Date", "date")
md[, Date := as.Date(date)]
setorder(md, Date); setkey(md, Date)
print(head(md))
 
stock_list <- setdiff(names(md), c("date","Date"))

detect_pelt_events_one <- function(sym, md, minseglen = 21L, gap = 5L) {
  dates <- md$Date
  x     <- as.numeric(md[[sym]])

  # 在去 NA 的索引上跑 PELT
  idx <- which(is.finite(x))
  if (length(idx) < 2L * minseglen) {
    # 数据太少，不足以做至少两段
    trigger <- data.table(Stock=sym, Date=dates, PELT_CP=0L)
    return(list(trigger=trigger, events=data.table(Stock=sym)[0]))
  }

  x_clean <- x[idx]

  # PELT (MBIC) on mean+var changes
  cp <- tryCatch(
    cpt.meanvar(x_clean,
                method   = "PELT",
                penalty  = "MBIC",
                minseglen= minseglen,
                class    = TRUE),
    error = function(e) NULL
  )

  # 逐日触发（切点为 1）
  PELT_CP <- integer(length(x))
  if (!is.null(cp)) {
    cp_idx <- cpts(cp)                # 切点的“右端”索引，包含末尾 n
    cp_idx <- cp_idx[cp_idx < length(x_clean)]  # 去掉末尾 n
    if (length(cp_idx)) {
      hit_pos <- idx[cp_idx]          # 映射回原始时间轴
      PELT_CP[hit_pos] <- 1L
    }
  }
  trigger <- data.table(Stock=sym, Date=dates, PELT_CP=PELT_CP)

  # 合并相邻 ≤ gap 天 的切点为“本地事件”
  hit_dates <- trigger[PELT_CP==1L, Date]
  if (length(hit_dates)==0L) {
    return(list(trigger=trigger, events=data.table(Stock=sym)[0]))
  }

  hit_dates <- sort(unique(as.Date(hit_dates)))
  gaps <- as.integer(diff(hit_dates))
  grp  <- 1L + c(0L, cumsum(gaps > gap))

  ev <- data.table(Date=hit_dates, grp=grp)[
          , .(start=min(Date), end=max(Date)), by=grp
        ][
          , center := as.Date(floor((as.numeric(start)+as.numeric(end))/2),
                              origin="1970-01-01")
        ]

  # 强度：该事件内的“切点个数”
  ev_det <- ev[, {
    sub <- trigger[Date>=start & Date<=end]
    .(PELT_CP_n = sum(sub$PELT_CP==1L))
  }, by=.(grp, start, end, center)]

  # 票数：此处只有 PELT 一种方法 → 设 1（方便后续与其它方法并表）
  ev_det[, `:=`(Stock=sym,
                votes=1L,
                event_id=paste(sym, grp, sep=":"))]
  setcolorder(ev_det, c("Stock","grp","start","end","center","PELT_CP_n","votes","event_id"))

  list(trigger=trigger, events=ev_det)
}


pelt_list <- lapply(stock_list, function(sym) {
  message("PELT ==> ", sym)
  tryCatch(detect_pelt_events_one(sym, md, minseglen=21L, gap=5L),
           error = function(e){ message("  [skip] ", sym, " : ", e$message); NULL })
})

pelt_events_all   <- rbindlist(lapply(pelt_list, `[[`, "events"),  use.names=TRUE, fill=TRUE)
pelt_triggers_all <- rbindlist(lapply(pelt_list, `[[`, "trigger"), use.names=TRUE, fill=TRUE)

# 简要检查
print(head(pelt_events_all))
pelt_summary_by_stock <- pelt_events_all[
  , .(
      events      = .N,
      median_len  = as.numeric(median(as.integer(end - start) + 1L)),
      median_hits = as.numeric(median(PELT_CP_n))
    ),
  by = Stock
][order(-events)]
print(pelt_summary_by_stock)

merge_local_events_to_global_pelt <- function(events_all,
                                              pad_days = 5L,
                                              min_hits = 1L,
                                              min_duration = 1L,
                                              per_stock_dedup = c("keep_max_hits","keep_earliest_center","none")) {
  stopifnot(all(c("Stock","start","end","center","PELT_CP_n") %in% names(events_all)))
  per_stock_dedup <- match.arg(per_stock_dedup)

  dt <- copy(events_all)[
    PELT_CP_n >= min_hits &
    (as.integer(end - start) + 1L) >= min_duration
  ]
  if (nrow(dt) == 0L) return(list(global_events=NULL, members=NULL))

  dt[, `:=`(gstart = start - pad_days, gend = end + pad_days)]
  setorder(dt, gstart, gend)

  grp <- integer(nrow(dt)); g <- 1L; cur_end <- dt$gend[1]; grp[1] <- g
  if (nrow(dt) > 1L) for (i in 2:nrow(dt)) {
    if (dt$gstart[i] <= cur_end) { grp[i] <- g; if (dt$gend[i] > cur_end) cur_end <- dt$gend[i] }
    else { g <- g + 1L; grp[i] <- g; cur_end <- dt$gend[i] }
  }
  dt[, global_grp := grp]

  members <- switch(per_stock_dedup,
    keep_max_hits       = dt[, .SD[which.max(PELT_CP_n)], by = .(global_grp, Stock)],
    keep_earliest_center= dt[, .SD[which.min(center)],   by = .(global_grp, Stock)],
    none                = copy(dt)
  )

  global_events <- members[, .(
    start         = min(start),
    end           = max(end),
    duration      = as.integer(max(end) - min(start)) + 1L,
    center_mean   = as.Date(round(mean(as.numeric(center))),   origin="1970-01-01"),
    center_median = as.Date(round(median(as.numeric(center))), origin="1970-01-01"),
    n_members     = .N,
    n_stocks      = uniqueN(Stock),
    stocks        = paste(sort(unique(Stock)), collapse = ","),
    hits_sum      = sum(PELT_CP_n),
    hits_max      = max(PELT_CP_n),
    hits_median   = as.numeric(median(PELT_CP_n))
  ), by = global_grp][order(start, center_median)]

  global_events[, global_id := sprintf("G%04d", seq_len(.N))]
  members <- merge(members, global_events[, .(global_grp, global_id)], by="global_grp", all.x=TRUE)
  setcolorder(members, c("global_id","global_grp","Stock","grp","start","end","center","PELT_CP_n"))

  list(global_events = global_events, members = members)
}

# 使用：
global_pelt <- merge_local_events_to_global_pelt(pelt_events_all, pad_days=5L, min_hits=1L)
print(head(global_pelt$global_events))


library(ggplot2)
library(data.table)

# --- Data from your pipeline ---
ge_pelt  <- copy(global_pelt$global_events)   # global events (PELT)
mem_pelt <- copy(global_pelt$members)         # per-stock local events participating those globals

# Order stocks by number of PELT events (readability)
ord_pelt <- pelt_events_all[, .N, by = Stock][order(-N, Stock)]$Stock
mem_pelt[, Stock := factor(Stock, levels = rev(ord_pelt))]

# --- Year ticks: full years on x-axis + faint annual grid lines ---
start_year <- as.Date(sprintf("%d-01-01", as.integer(format(min(md$Date), "%Y"))))
end_year   <- as.Date(sprintf("%d-01-01", as.integer(format(max(md$Date), "%Y")) + 1))
year_breaks <- seq(start_year, end_year, by = "1 year")
half_breaks <- seq(start_year, end_year, by = "6 months")
yr_grid     <- data.table(breaks = year_breaks)

# --- Plot: local windows (segments) + local centers (squares) + global centers (dashed) ---
p_pelt <- ggplot(mem_pelt) +
  # faint yearly background lines for readability
  geom_vline(data = yr_grid, aes(xintercept = as.numeric(breaks)),
             color = "grey92", linewidth = 0.3, inherit.aes = FALSE) +
  # local event window per stock
  geom_segment(aes(x = start, xend = end, y = Stock, yend = Stock, color = PELT_CP_n),
               linewidth = 3, lineend = "round", alpha = 0.95) +
  # local event center (square points)
  geom_point(aes(x = center, y = Stock, fill = PELT_CP_n),
             shape = 22, size = 2.8, color = "black", stroke = 0.2, alpha = 0.95) +
  # global centers (dashed grey)
  geom_vline(data = ge_pelt, aes(xintercept = as.numeric(center_median)),
             linetype = "dashed", linewidth = 0.5, alpha = 0.45, inherit.aes = FALSE) +

  # legends (English)
  scale_color_viridis_c(name = "Intensity (break count)") +
  scale_fill_viridis_c(name  = "Intensity (break count)") +

  # axis: show every year & minor ticks every 6 months
  scale_x_date(breaks = year_breaks, minor_breaks = half_breaks, date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Local Events by Stock with Global Centers (PELT, MBIC, minseglen = 21)",
       x = "Year", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),  # we draw our own faint year lines
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# # show on screen
# print(p_pelt)

# # save as a square figure
# ggsave("gantt_pelt_square_en.png", plot = p_pelt,
#        width = 8, height = 5, units = "in", dpi = 300, bg = "white")
