library(data.table)
library(ecp)
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

library(data.table)
library(ecp)      # e.divisive 在这个包
# md, stock_list 已在你环境中

# ===== E.divisive：26 维联合检测（全局变点）=====
# detect_ecp_divisive_global <- function(md,
#                                        stocks,
#                                        min_size = 21L,     # 每段最短长度
#                                        R = 199,            # 置换次数（越大越慢）
#                                        sig = 0.05,         # 显著性水平
#                                        gap = 5L) {         # 合并相邻触发
#   stopifnot(all(stocks %in% names(md)), "Date" %in% names(md))
#   # 取 26 维矩阵，去掉含 NA 的行，保留日期对齐
#   X <- as.matrix(md[, ..stocks])
#   ok <- stats::complete.cases(X)
#   dates2 <- md$Date[ok]
#   X2 <- X[ok, , drop = FALSE]
#   n <- nrow(X2)
#   if (n < 2L*min_size) stop("样本太短，无法做至少两段（min_size=", min_size, "）")

#   set.seed(123)
#   res <- ecp::e.divisive(X = X2, sig.lvl = sig, R = R, min.size = min_size)

#   # --- 提取变点下标（去除首尾）
#   # e.divisive 的常用输出是 res$estimates，其中包含 1 和 n
#   cp_idx <- integer(0)
#   if (!is.null(res$estimates) && is.numeric(res$estimates)) {
#     cp_idx <- res$estimates
#   } else if (!is.null(res$estimates$cp.hat)) {
#     cp_idx <- res$estimates$cp.hat
#   } else if (!is.null(res$estimates$cp.loc)) {
#     cp_idx <- res$estimates$cp.loc
#   }
#   cp_idx <- setdiff(unique(sort(cp_idx)), c(1L, n))
#   cp_dates <- if (length(cp_idx)) dates2[cp_idx] else as.Date(character())

#   # --- 逐日触发表（在原始完整日期轴上打点）
#   trigger <- data.table(Date = md$Date, ECP_CP = 0L)
#   if (length(cp_dates)) trigger[Date %in% cp_dates, ECP_CP := 1L]

#   # --- 合并相邻 ≤ gap 天为“全局事件”
#   if (length(cp_dates) == 0L) {
#     events <- data.table()[, `:=`(start=as.Date(character()),
#                                   end=as.Date(character()),
#                                   center=as.Date(character()),
#                                   ECP_CP_n=integer())][0]
#   } else {
#     d <- sort(unique(as.Date(cp_dates)))
#     gaps <- as.integer(diff(d))
#     grp  <- 1L + c(0L, cumsum(gaps > gap))
#     events <- data.table(Date = d, grp = grp)[
#       , .(start = min(Date), end = max(Date)), by = grp
#     ][
#       , center := as.Date(floor((as.numeric(start) + as.numeric(end))/2),
#                           origin = "1970-01-01")
#     ][
#       , ECP_CP_n := 1L][]  # 每个事件至少含一个切点
#   }

#   # --- 打包一个与此前“global_*”类似的表，便于画全局竖线
#   if (nrow(events)) {
#     global_events <- events[, .(
#       start         = start,
#       end           = end,
#       duration      = as.integer(end - start) + 1L,
#       center_mean   = center,                 # 单点并并不影响
#       center_median = center,
#       n_members     = length(stocks),         # 多元 → 默认等同参与
#       n_stocks      = length(stocks),
#       stocks        = paste(sort(stocks), collapse = ","),
#       hits_sum      = ECP_CP_n,
#       hits_max      = ECP_CP_n,
#       hits_median   = ECP_CP_n
#     )][, `:=`(global_grp = .I,
#               global_id = sprintf("ECP%04d", .I))]
#   } else {
#     global_events <- data.table()
#   }

#   list(
#     res = res,                  # 原始 ecp 对象（可看 p.values / cluster）
#     cp_idx = cp_idx,            # 变点下标（在去 NA 的索引上）
#     cp_dates = cp_dates,        # 变点日期
#     trigger = trigger,          # 逐日触发（在完整日期轴）
#     events = events,            # 合并近邻后的“全局事件”
#     global_events = global_events
#   )
# }
detect_ecp_div_global <- function(md,
                                  stocks,
                                  min_size = 21L,   # e.divisive 的最小段
                                  R = 199,          # 置换次数
                                  sig = 0.05,       # 显著性
                                  alpha = 1,
                                  gap = 5L) {       # 合并近邻容差
  stopifnot(all(stocks %in% names(md)), "Date" %in% names(md))
  X <- as.matrix(md[, ..stocks])
  ok <- stats::complete.cases(X)
  dates2 <- md$Date[ok]
  X2 <- X[ok, , drop = FALSE]
  n  <- nrow(X2)
  if (n < 2L * min_size) stop("样本太短，无法做至少两段（min_size=", min_size, "）")

  set.seed(123)
  dv <- ecp::e.divisive(X = X2, R = R, sig.lvl = sig, alpha = alpha, min.size = min_size)

  cp_idx <- if (!is.null(dv$estimates)) unique(sort(dv$estimates)) else integer(0)
  cp_idx <- setdiff(cp_idx, c(1L, n))
  cp_dates <- if (length(cp_idx)) dates2[cp_idx] else as.Date(character())

  # 合并相邻 ≤ gap 天
  if (length(cp_dates)) {
    d <- sort(unique(as.Date(cp_dates)))
    grp  <- 1L + c(0L, cumsum(as.integer(diff(d)) > gap))
    events <- data.table(Date = d, grp = grp)[
      , .(start = min(Date), end = max(Date)), by = grp
    ][
      , center := as.Date(floor((as.numeric(start) + as.numeric(end))/2),
                          origin = "1970-01-01")
    ]
  } else {
    events <- data.table()
  }

  list(res = dv, cp_idx = cp_idx, cp_dates = cp_dates,
       events = events)
}

# 例子（和你 agglo 的参数对齐）：
ecp_div <- detect_ecp_div_global(md, stocks = stock_list,
                                 min_size = 21L, R = 199, sig = 0.05, gap = 5L)


# 用上面统一的抽取：
# pred_div <- normalize_pred_dates(ecp_div$cp_dates, market_dates)

# ===== 运行（用你已有的 md / stock_list）=====
# ecp_divisive <- detect_ecp_divisive_global(
#   md, stocks = stock_list,
#   min_size = 21L, R = 199, sig = 0.05, gap = 5L
# )
# print(ecp_divisive)
# 看看结果
# print(ecp_divisive$cp_dates)
# print(ecp_divisive$events)
# print(ecp_divisive$global_events)



library(data.table)
library(ggplot2)

ge <- copy(ecp_div$global_events)
stopifnot(nrow(ge) > 0)

start_year  <- as.Date(sprintf("%d-01-01", as.integer(format(min(md$Date), "%Y"))))
end_year    <- as.Date(sprintf("%d-01-01", as.integer(format(max(md$Date), "%Y")) + 1))
year_breaks <- seq(start_year, end_year, by = "1 year")
half_breaks <- seq(start_year, end_year, by = "6 months")

# 用空白基底，只保留 X 轴
base <- data.table(Date = c(min(md$Date), max(md$Date)), y = 0)

p_centers <- ggplot(base, aes(Date, y)) +
  geom_blank() +
  geom_vline(data = ge, aes(xintercept = as.numeric(center_median)),
             linetype = "dashed", linewidth = 0.7, alpha = 0.85) +
  scale_x_date(breaks = year_breaks, minor_breaks = half_breaks, date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Global Change-Point Centers (E.divisive, 26-D)",
       x = "Year", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

print(p_centers)
ggsave("ecp_global_centers_square.png", p_centers,
       width = 10, height = 10, units = "in", dpi = 300, bg = "white")

