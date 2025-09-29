
library(data.table)
library(depmixS4)
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
# 1) 小工具：对 Viterbi 状态序列做最小停留时长滤波（反复合并短段）
.prune_states <- function(states, min_run = 5L) {
  stopifnot(min_run >= 1L)
  if (min_run <= 1L) return(states)
  repeat {
    r <- rle(states)
    idx <- which(r$lengths < min_run)
    if (length(idx) == 0L) break
    for (j in idx) {
      # 开头短段并向后；其他短段并向前（也可更精细地比较前后长度）
      r$values[j] <- if (j == 1L) r$values[j+1L] else r$values[j-1L]
    }
    states <- inverse.rle(r)
  }
  states
}

# 2) 选k=2或3的最优HMM（BIC）
.fit_hmm_best <- function(x, k_candidates = c(2L,3L)) {
  best <- NULL
  for (k in k_candidates) {
    fit_k <- tryCatch({
      m <- depmixS4::depmix(response = x ~ 1, family = gaussian(), nstates = k,
                            data = data.frame(x = x))
      tmp <- utils::capture.output(fit <- depmixS4::fit(m, verbose = FALSE))
      list(fit = fit, k = k, bic = BIC(fit))
    }, error = function(e) NULL)
    if (!is.null(fit_k)) if (is.null(best) || fit_k$bic < best$bic) best <- fit_k
  }
  best
}

# 3) 单股检测：特征=abs/ret/r2；Viterbi + 最小停留；强度=切换数&高波动天数
detect_hmm_events_one <- function(sym, md,
                                  feature = c("abs","ret","r2"),
                                  k_candidates = c(2L,3L),
                                  min_len = 60L,
                                  min_run = 7L,
                                  gap = 3L) {
  feature <- match.arg(feature)
  dates <- md$Date
  r <- as.numeric(md[[sym]])

  idx <- which(is.finite(r))
  if (length(idx) < min_len) {
    trig <- data.table(Stock = sym, Date = dates, HMM_CP = 0L, HMM_k = NA_integer_, State = NA_integer_)
    return(list(trigger = trig, events = data.table(Stock = sym)[0]))
  }

  # 特征构造：默认绝对收益，更聚焦波动状态
  x <- switch(feature,
              abs = abs(r[idx]),
              ret = r[idx],
              r2  = r[idx]^2)

  # 标准化以稳定拟合（线性缩放不破坏相对结构）
  x <- as.numeric(scale(x)); x[!is.finite(x)] <- 0

  best <- .fit_hmm_best(x, k_candidates)
  HMM_CP <- integer(length(r)); HMM_k <- NA_integer_; viterbi_states <- rep(NA_integer_, length(r))
  if (!is.null(best)) {
    post   <- depmixS4::posterior(best$fit, type = "viterbi")
    states <- post$state

    # 最小停留滤波（降噪的关键一步）
    states_s <- .prune_states(states, min_run = min_run)

    # 切换点（滤波后）
    trans <- c(0L, as.integer(diff(states_s) != 0L))
    HMM_CP[idx] <- trans
    HMM_k <- best$k
    viterbi_states[idx] <- states_s
  }

  # 定义“高波动态”：按该特征在各态上的均值/方差来识别
  # 这里直接用滤波后状态的组内均值来排序（x 已标准化）
  high_state <- NA_integer_
  high_day   <- integer(length(r))
  if (sum(!is.na(viterbi_states)) > 0) {
    mu_by_state <- tapply(x, viterbi_states[!is.na(viterbi_states)], mean, na.rm = TRUE)
    if (!is.null(mu_by_state)) {
      high_state <- as.integer(names(which.max(mu_by_state))[1])
      high_day[idx] <- as.integer(viterbi_states[idx] == high_state)
    }
  }

  trigger <- data.table(Stock = sym, Date = dates,
                        HMM_CP = HMM_CP, HMM_k = HMM_k,
                        State = viterbi_states,
                        HVOL = high_day)

  # 合并相邻 ≤ gap 天 的切换为“本地事件”
  hit_dates <- trigger[HMM_CP == 1L, Date]
  if (length(hit_dates) == 0L) {
    return(list(trigger = trigger, events = data.table(Stock = sym)[0]))
  }

  hit_dates <- sort(unique(as.Date(hit_dates)))
  gaps <- as.integer(diff(hit_dates))
  grp  <- 1L + c(0L, cumsum(gaps > gap))

  ev <- data.table(Date = hit_dates, grp = grp)[
          , .(start = min(Date), end = max(Date)), by = grp
        ][
          , center := as.Date(floor((as.numeric(start) + as.numeric(end)) / 2),
                              origin = "1970-01-01")
        ]

  # 强度1：窗口内切换次数；强度2：高波动态天数
  ev_det <- ev[, {
    sub <- trigger[Date >= start & Date <= end]
    .(HMM_sw_n  = sum(sub$HMM_CP == 1L, na.rm = TRUE),   # switches
      HMM_hv_n  = sum(sub$HVOL   == 1L, na.rm = TRUE))   # high-vol days
  }, by = .(grp, start, end, center)]

  ev_det[, `:=`(Stock = sym,
                votes = 1L,
                event_id = paste(sym, grp, sep = ":"))]
  data.table::setcolorder(ev_det, c("Stock","grp","start","end","center","HMM_sw_n","HMM_hv_n","votes","event_id"))

  list(trigger = trigger, events = ev_det)
}

# 批量
hmm_list <- lapply(stock_list, function(sym) {
  message("HMM(sticky) ==> ", sym)
  tryCatch(detect_hmm_events_one(sym, md,
                                 feature = "abs",   # 可选 "ret"/"r2"
                                 k_candidates = c(2L,3L),
                                 min_len = 60L,
                                 min_run = 5L,
                                 gap = 5L),
           error = function(e){ message("  [skip] ", sym, " : ", e$message); NULL })
})

hmm_events_all   <- rbindlist(lapply(hmm_list, `[[`, "events"),  use.names = TRUE, fill = TRUE)
hmm_triggers_all <- rbindlist(lapply(hmm_list, `[[`, "trigger"), use.names = TRUE, fill = TRUE)

# 概览（用两个强度都看一眼）
print(head(hmm_events_all))
summary_by_stock <- hmm_events_all[
  , .(
      events        = .N,
      dur_median    = as.numeric(median(as.integer(end - start) + 1L)),
      sw_median     = as.numeric(median(HMM_sw_n)),
      hvdays_median = as.numeric(median(HMM_hv_n))
    ),
  by = Stock
][order(-events)]
print(summary_by_stock)

merge_local_events_to_global_hmm <- function(events_all,
                                             pad_days = 5L,
                                             min_hits = 1L,
                                             min_duration = 1L,
                                             use = c("switch","hvol"),
                                             per_stock_dedup = c("keep_max_hits","keep_earliest_center","none")) {
  use <- match.arg(use); per_stock_dedup <- match.arg(per_stock_dedup)
  score_col <- if (use == "switch") "HMM_sw_n" else "HMM_hv_n"
  stopifnot(all(c("Stock","start","end","center",score_col) %in% names(events_all)))

  dt <- copy(events_all)[ get(score_col) >= min_hits &
                          (as.integer(end - start) + 1L) >= min_duration ]
  if (nrow(dt) == 0L) return(list(global_events = NULL, members = NULL))

  dt[, `:=`(gstart = start - pad_days, gend = end + pad_days)]
  setorder(dt, gstart, gend)

  grp <- integer(nrow(dt)); g <- 1L; cur_end <- dt$gend[1]; grp[1] <- g
  if (nrow(dt) > 1L) for (i in 2:nrow(dt)) {
    if (dt$gstart[i] <= cur_end) { grp[i] <- g; if (dt$gend[i] > cur_end) cur_end <- dt$gend[i] }
    else { g <- g + 1L; grp[i] <- g; cur_end <- dt$gend[i] }
  }
  dt[, global_grp := grp]

  members <- switch(per_stock_dedup,
    keep_max_hits        = dt[, .SD[which.max(get(score_col))], by = .(global_grp, Stock)],
    keep_earliest_center = dt[, .SD[which.min(center)],         by = .(global_grp, Stock)],
    none                 = copy(dt)
  )

  global_events <- members[, .(
    start         = min(start),
    end           = max(end),
    duration      = as.integer(max(end) - min(start)) + 1L,
    center_mean   = as.Date(round(mean(as.numeric(center))),   origin = "1970-01-01"),
    center_median = as.Date(round(median(as.numeric(center))), origin = "1970-01-01"),
    n_members     = .N,
    n_stocks      = uniqueN(Stock),
    stocks        = paste(sort(unique(Stock)), collapse = ","),
    hits_sum      = sum(get(score_col)),
    hits_max      = max(get(score_col)),
    hits_median   = as.numeric(median(get(score_col)))
  ), by = global_grp][order(start, center_median)]

  global_events[, global_id := sprintf("G%04d", seq_len(.N))]
  members <- merge(members, global_events[, .(global_grp, global_id)], by = "global_grp", all.x = TRUE)

  list(global_events = global_events, members = members)
}

# 用高波动态天数作为“强度”
global_hmm <- merge_local_events_to_global_hmm(hmm_events_all, pad_days = 5L, min_hits = 1L, use = "hvol")

ge  <- copy(global_hmm$global_events)
mem <- copy(global_hmm$members)

ord <- hmm_events_all[, .N, by = Stock][order(-N, Stock)]$Stock
mem[, Stock := factor(Stock, levels = rev(ord))]

start_year  <- as.Date(sprintf("%d-01-01", as.integer(format(min(md$Date), "%Y"))))
end_year    <- as.Date(sprintf("%d-01-01", as.integer(format(max(md$Date), "%Y")) + 1))
year_breaks <- seq(start_year, end_year, by = "1 year")
half_breaks <- seq(start_year, end_year, by = "6 months")
yr_grid     <- data.table(breaks = year_breaks)

library(ggplot2)
p <- ggplot(mem) +
  geom_vline(data = yr_grid, aes(xintercept = as.numeric(breaks)),
             color = "grey92", linewidth = 0.3, inherit.aes = FALSE) +
  geom_segment(aes(x = start, xend = end, y = Stock, yend = Stock, color = HMM_hv_n),
               linewidth = 3, lineend = "round", alpha = 0.95) +
  geom_point(aes(x = center, y = Stock, fill = HMM_hv_n),
             shape = 22, size = 2.8, color = "black", stroke = 0.2, alpha = 0.95) +
  geom_vline(data = ge, aes(xintercept = as.numeric(center_median)),
             linetype = "dashed", linewidth = 0.5, alpha = 0.45, inherit.aes = FALSE) +
  scale_color_viridis_c(name = "Intensity (high-vol days)") +
  scale_fill_viridis_c(name  = "Intensity (high-vol days)") +
  scale_x_date(breaks = year_breaks, minor_breaks = half_breaks, date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Local Events by Stock with Global Centers (HMM, Viterbi, sticky, 2–3 states)",
       x = "Year", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5))

# print(p)
# ggsave("gantt_hmm_sticky_hvol_square.png", p, width = 8, height = 5, units = "in", dpi = 300, bg = "white")
