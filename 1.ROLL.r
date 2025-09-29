library(data.table)
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

detect_roll_events_one <- function(sym, md, width = 21, q = 0.98, gap = 5L) {
  dates <- md$Date
  x <- as.numeric(md[[sym]])                         # 日收益率序列

  # 21日滚动标准差 & 98%阈值
  roll_sd <- zoo::rollapply(x, width = width, FUN = function(z) sd(z, na.rm = TRUE),
                            align = "right", fill = NA)
  thr <- quantile(roll_sd, q, na.rm = TRUE)
  cp  <- as.integer(roll_sd > thr); cp[is.na(cp)] <- 0L

  # 逐日触发表（可选保存/画图用）
  trigger <- data.table(Stock = sym, Date = dates,
                        ROLL_SD = roll_sd, THR = thr, ROLL_CP = cp)

  # 命中日
  hit_dates <- trigger[ROLL_CP == 1L, Date]
  if (length(hit_dates) == 0L) {
    return(list(trigger = trigger,
                events  = data.table(Stock = sym)[0]))   # 空表
  }

  # ✅ 用 sort() 而不是 setorder()（因为这是向量，不是 data.table）
  hit_dates <- sort(unique(hit_dates))

  # 合并相邻≤ gap 天
  # 用 as.integer(diff(...)) 计算相邻间隔（单位：天）
  gaps <- as.integer(diff(hit_dates))
  grp  <- 1L + c(0L, cumsum(gaps > gap))

   ev  <- data.table(Date = hit_dates, grp = grp)[
           , .(start = min(Date), end = max(Date)), by = grp
         ][
           , center := as.Date(floor((as.numeric(start) + as.numeric(end))/2),
                               origin = "1970-01-01")
         ]

  # ev  <- data.table(Date = hit_dates, grp = grp)[,
  #         .(start = min(Date), end = max(Date)), by = grp]
  # ev[, center := as.Date(floor((as.numeric(start) + as.numeric(end)) / 2),
  #                        origin = "1970-01-01")]

  # 事件窗口内的 Rolling 触发天数（强度）
  ev_det <- ev[, {
    sub <- trigger[Date >= start & Date <= end]
    .(ROLL_CP_n = sum(sub$ROLL_CP == 1L, na.rm = TRUE))
  }, by = .(grp, start, end, center)]

  # 票数：此处只有 Rolling 一种方法 → 设 1
  # ev_det[, `:=`(Stock = sym,
  #               votes = 1L,
  #               event_id = paste(sym, grp, sep = ":"))]
  # setcolorder(ev_det, c("Stock","grp","start","end","center","ROLL_CP_n","votes","event_id"))
  # list(trigger = trigger, events = ev_det)
   ev_det <- ev[, {
    sub <- trigger[Date >= start & Date <= end]
    .(ROLL_CP_n = sum(sub$ROLL_CP == 1L, na.rm = TRUE))
  }, by = .(grp, start, end, center)]

  # 票数：此处仅 Rolling 一种方法 → 设 1
  ev_det[, `:=`(Stock = sym,
                votes = 1L,
                event_id = paste(sym, grp, sep = ":"))]
  setcolorder(ev_det, c("Stock","grp","start","end","center","ROLL_CP_n","votes","event_id"))

  list(trigger = trigger, events = ev_det)
}

res_list <- lapply(stock_list, function(sym) {
  message("==> ", sym)
  tryCatch(detect_roll_events_one(sym, md, width = 21, q = 0.98, gap = 5L),
           error = function(e) { message("  [跳过] ", sym, " : ", e$message); NULL })
})

# 汇总：事件表 & 每日触发表（可选）
events_all   <- rbindlist(lapply(res_list, `[[`, "events"),  use.names = TRUE, fill = TRUE)
triggers_all <- rbindlist(lapply(res_list, `[[`, "trigger"), use.names = TRUE, fill = TRUE)


summary_by_stock <- events_all[
  , .(
      events      = .N,
      median_len  = as.numeric(median(as.integer(end - start) + 1L)),
      median_hits = as.numeric(median(ROLL_CP_n))
    ),
  by = Stock
][order(-events)]
print(summary_by_stock)


library(data.table)

# 把各股本地事件合并为“全局事件”
merge_local_events_to_global <- function(events_all,
                                         pad_days = 5L,         # 跨股合并容差
                                         min_hits = 1L,         # 进入全局合并的最小强度（ROLL_CP_n）
                                         min_duration = 1L,     # 最小持续天数
                                         per_stock_dedup = c("keep_max_hits", "keep_earliest_center", "none")) {
  stopifnot(all(c("Stock","start","end","center","ROLL_CP_n") %in% names(events_all)))
  per_stock_dedup <- match.arg(per_stock_dedup)

  # 1) 过滤弱事件（可按需调整门槛）
  dt <- copy(events_all)[
    ROLL_CP_n >= min_hits &
    (as.integer(end - start) + 1L) >= min_duration
  ]
  if (nrow(dt) == 0L) return(list(global_events=NULL, members=NULL))

  # 2) 外扩容差窗口；按起点排序
  dt[, `:=`(gstart = start - pad_days,
            gend   = end   + pad_days)]
  setorder(dt, gstart, gend)

  # 3) 线性扫描合并重叠区间 -> global_grp
  grp <- integer(nrow(dt))
  g <- 1L
  cur_end <- dt$gend[1]
  grp[1] <- g
  if (nrow(dt) > 1L) {
    for (i in 2:nrow(dt)) {
      if (dt$gstart[i] <= cur_end) {
        grp[i] <- g
        if (dt$gend[i] > cur_end) cur_end <- dt$gend[i]
      } else {
        g <- g + 1L
        grp[i] <- g
        cur_end <- dt$gend[i]
      }
    }
  }
  dt[, global_grp := grp]

  # 4) 每个全局组内，如同一只股票出现多次，决定保留规则
  members <- switch(per_stock_dedup,
    keep_max_hits = dt[, .SD[which.max(ROLL_CP_n)], by = .(global_grp, Stock)],
    keep_earliest_center = dt[, .SD[which.min(center)], by = .(global_grp, Stock)],
    none = copy(dt)
  )

  # 5) 汇总全局事件（中心：均值/中位数；强度/规模）
  global_events <- members[, .(
    start         = min(start),
    end           = max(end),
    duration      = as.integer(max(end) - min(start)) + 1L,
    center_mean   = as.Date(round(mean(as.numeric(center))), origin = "1970-01-01"),
    center_median = as.Date(round(median(as.numeric(center))), origin = "1970-01-01"),
    n_members     = .N,                                # 参与的（股票×本地事件）条数
    n_stocks      = uniqueN(Stock),                    # 参与股票数
    stocks        = paste(sort(unique(Stock)), collapse = ","),
    hits_sum      = sum(ROLL_CP_n),
    hits_max      = max(ROLL_CP_n),
    hits_median   = as.numeric(median(ROLL_CP_n))
  ), by = global_grp]

  setorder(global_events, start, center_median)
  global_events[, global_id := sprintf("G%04d", seq_len(.N))]

  # 6) 把 global_id 回填给成员表
  members <- merge(members, global_events[, .(global_grp, global_id)], by="global_grp", all.x=TRUE)
  setcolorder(members, c("global_id","global_grp","Stock","grp","start","end","center","ROLL_CP_n"))

  list(global_events = global_events, members = members)
}

# ==== 使用示例 ====
# 你已有的本地事件表：events_all
global_roll <- merge_local_events_to_global(
  events_all,
  pad_days = 5L,          # 与你本地合并gap一致，常用 3~7
  min_hits = 1L,          # 只要有触发就参与；也可提到 2/3 变更严格
  per_stock_dedup = "keep_max_hits"  # 同股多事件时保留强度最大者
)

# 看结果
# print(head(global_roll$global_events))
# print(head(global_roll$members))

# # 画全局事件竖线叠任意一只股票（例：xlf）
# if (!is.null(global_roll$global_events)) {
#   library(ggplot2)
#   pdat <- data.table(Date = md$Date, Ret = md[["xlf"]])  # 你的 multi_data 已是收益率
#   ggplot(pdat, aes(Date, Ret)) +
#     geom_line(color = "gray40") +
#     geom_vline(data = global_roll$global_events,
#                aes(xintercept = as.numeric(center_median)),
#                linetype = "dashed", linewidth = 0.6, alpha = 0.9) +
#     labs(title = "Global Events (Rolling-Std based)", y = "Daily Return (xlf)", x = "Date") +
#     theme_minimal()
# }



stopifnot(!is.null(global_roll$members))
mem <- copy(global_roll$members)

# 按“事件数”给股票排序，便于阅读
ord <- events_all[, .N, by = Stock][order(-N)]$Stock
mem[, Stock := factor(Stock, levels = rev(ord))]

# ggplot(mem) +
#   geom_segment(aes(x = start, xend = end, y = Stock, yend = Stock, color = ROLL_CP_n),
#                linewidth = 3, lineend = "round") +
#   geom_vline(data = global$global_events,
#              aes(xintercept = as.numeric(center_median)),
#              linetype = "dashed", alpha = 0.35, inherit.aes = FALSE) +
#   scale_color_viridis_c(name = "强度：\n窗口内触发天数") +
#   labs(title = "各股本地事件与全局中心（Rolling-Std 结果）",
#        x = "Date", y = NULL) +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position = "right")


# library(ggplot2)
# library(data.table)

# # 1) 准备数据（按事件数给股票排序，阅读性更好）
# ge  <- copy(global$global_events)
# mem <- copy(global$members)

# ord <- events_all[, .N, by = Stock][order(-N, Stock)]$Stock
# mem[, Stock := factor(Stock, levels = rev(ord))]

# # 2) 画图（甘特图：线段=本地窗口；方块=本地中心；虚线=全局中心）
# p <- ggplot(mem) +
#   # 本地事件窗口
#   geom_segment(aes(x = start, xend = end, y = Stock, yend = Stock,
#                    color = ROLL_CP_n),
#                linewidth = 3, lineend = "round", alpha = 0.95) +
#   # 本地事件中心（方形点）
#   geom_point(aes(x = center, y = Stock, fill = ROLL_CP_n),
#              shape = 22, size = 2.8, color = "black", stroke = 0.2, alpha = 0.95) +
#   # 全局事件中心（灰色虚线）
#   geom_vline(data = ge, aes(xintercept = as.numeric(center_median)),
#              linetype = "dashed", linewidth = 0.5, alpha = 0.35, inherit.aes = FALSE) +

#   # 颜色与填充（同一尺度，便于阅读）
#   scale_color_viridis_c(name = "强度：触发天数", option = "D") +
#   scale_fill_viridis_c(name  = "强度：触发天数", option = "D") +

#   # 轴与主题
#   scale_x_date(expand = expansion(mult = c(0.01, 0.02))) +
#   labs(title = "各股本地事件与全局中心（Rolling-Std）",
#        x = "Date", y = NULL) +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid.major.y = element_blank(),
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5)
#   )

# # 3) 屏幕查看
# print(p)

# # 4) 正方形版面导出（关键：宽=高）
# ggsave("gantt_global_square.png", plot = p,
#        width = 10, height = 10, units = "in", dpi = 300, bg = "white")

library(ggplot2)
library(data.table)

# Data
ge  <- copy(global_roll$global_events)
mem <- copy(global_roll$members)

# Order stocks by number of events (descending) for readability
ord <- events_all[, .N, by = Stock][order(-N, Stock)]$Stock
mem[, Stock := factor(Stock, levels = rev(ord))]

# Plot (square layout; English labels/legends)
p <- ggplot(mem) +
  # local event window
  geom_segment(aes(x = start, xend = end, y = Stock, yend = Stock,
                   color = ROLL_CP_n),
               linewidth = 3, lineend = "round", alpha = 0.95) +
  # local event center (square points)
  geom_point(aes(x = center, y = Stock, fill = ROLL_CP_n),
             shape = 22, size = 2.8, color = "black", stroke = 0.2, alpha = 0.95) +
  # global event centers (dashed grey)
  geom_vline(data = ge, aes(xintercept = as.numeric(center_median)),
             linetype = "dashed", linewidth = 0.5, alpha = 0.35, inherit.aes = FALSE) +

  # Legends in English
  scale_color_viridis_c(name = "Intensity (trigger days)", option = "D") +
  scale_fill_viridis_c(name  = "Intensity (trigger days)", option = "D") +

  scale_x_date(expand = expansion(mult = c(0.01, 0.02))) +
  labs(
    title = "Local Events by Stock with Global Centers (Rolling Std)",
    x = "Date", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Show
# print(p)

# # Save as a square figure (PNG + PDF)
# ggsave("gantt_global_rolling_square_en.png", plot = p,
#        width = 8, height = 5, units = "in", dpi = 300, bg = "white")
