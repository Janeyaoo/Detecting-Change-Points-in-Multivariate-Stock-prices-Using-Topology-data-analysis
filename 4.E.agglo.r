library(data.table)
library(ecp)
data_path <- "/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/multi_log_return_data.csv"
multi_data <- fread(data_path)

md <- copy(multi_data)
if ("V1" %in% names(md)) md[, V1 := NULL]
if ("Date" %in% names(md) && !"date" %in% names(md)) setnames(md, "Date", "date")
md[, Date := as.Date(date)]
setorder(md, Date); setkey(md, Date)
print(head(md))
 
stock_list <- setdiff(names(md), c("date","Date"))

detect_ecp_agglo_global <- function(md,
                                    stocks,
                                    min_size = 21L,   # 用 member 做初始分段控制分辨率
                                    gap = 5L,         # 合并近邻容差
                                    alpha = 1,
                                    penalty_lambda = 0 # 对切点数量的线性罚项强度
) {
  stopifnot(all(stocks %in% names(md)), "Date" %in% names(md))
  X <- as.matrix(md[, ..stocks])
  ok <- stats::complete.cases(X)
  dates2 <- md$Date[ok]
  X2 <- X[ok, , drop = FALSE]
  n  <- nrow(X2)
  if (n < 2L * min_size) stop("样本太短，无法做至少两段（min_size=", min_size, "）")

  # 关键：用 member 做初始分段（≈每段 ~min_size 天）
  member <- as.integer(ceiling(seq_len(n) / min_size))

  # 罚项：对切点个数线性惩罚（可设为 0 表示不惩罚）
  pen_fun <- function(cps, ...) penalty_lambda * length(cps)

  set.seed(123)
  ag <- ecp::e.agglo(X = X2, member = member, alpha = alpha, penalty = pen_fun)

  # 取变点（去掉首尾）
  cp_idx <- if (!is.null(ag$estimates)) unique(sort(ag$estimates)) else integer(0)
  cp_idx <- setdiff(cp_idx, c(1L, n))
  cp_dates <- if (length(cp_idx)) dates2[cp_idx] else as.Date(character())

  # 逐日触发表（映回完整日期轴）
  trigger <- data.table(Date = md$Date, ECP_AG_CP = 0L)
  if (length(cp_dates)) trigger[Date %in% cp_dates, ECP_AG_CP := 1L]

  # 合并相邻 ≤ gap 天 为全局事件
  if (length(cp_dates) == 0L) {
    events <- data.table()[, `:=`(start=as.Date(character()),
                                  end=as.Date(character()),
                                  center=as.Date(character()),
                                  ECP_AG_CP_n=integer())][0]
  } else {
    d <- sort(unique(as.Date(cp_dates)))
    grp  <- 1L + c(0L, cumsum(as.integer(diff(d)) > gap))
    events <- data.table(Date = d, grp = grp)[
      , .(start = min(Date), end = max(Date)), by = grp
    ][
      , center := as.Date(floor((as.numeric(start) + as.numeric(end))/2),
                          origin = "1970-01-01")
    ][
      , ECP_AG_CP_n := 1L][]
  }

  # 打包全局事件（与之前风格一致）
  global_events <- if (nrow(events)) {
    events[, .(
      start         = start,
      end           = end,
      duration      = as.integer(end - start) + 1L,
      center_mean   = center,
      center_median = center,
      n_members     = length(stocks),
      n_stocks      = length(stocks),
      stocks        = paste(sort(stocks), collapse = ","),
      hits_sum      = ECP_AG_CP_n,
      hits_max      = ECP_AG_CP_n,
      hits_median   = ECP_AG_CP_n
    )][, `:=`(global_grp = .I,
              global_id = sprintf("EAG%04d", .I))]
  } else data.table()[0]

  list(res = ag,
       cp_idx = cp_idx,
       cp_dates = cp_dates,
       trigger = trigger,
       events = events,
       global_events = global_events)
}

# 调用：
ecp_agglo <- detect_ecp_agglo_global(
  md, stocks = stock_list,
  min_size = 63L, gap = 21L,
  alpha = 1, penalty_lambda = 0  # 可把 penalty_lambda 调到 1~5 抑制过多切点
)

print(ecp_agglo)

# library(data.table)
# library(ggplot2)
# ge <- copy(ecp_agglo$global_events)
# if (nrow(ge) > 0) {
#   start_year  <- as.Date(sprintf("%d-01-01", as.integer(format(min(md$Date), "%Y"))))
#   end_year    <- as.Date(sprintf("%d-01-01", as.integer(format(max(md$Date), "%Y")) + 1))
#   year_breaks <- seq(start_year, end_year, by = "1 year")
#   half_breaks <- seq(start_year, end_year, by = "6 months")

#   base <- data.table(Date = c(min(md$Date), max(md$Date)), y = 0)
#   p_centers <- ggplot(base, aes(Date, y)) +
#     geom_blank() +
#     geom_vline(data = ge, aes(xintercept = as.numeric(center_median)),
#                linetype = "dashed", linewidth = 0.7, alpha = 0.85) +
#     scale_x_date(breaks = year_breaks, minor_breaks = half_breaks, date_labels = "%Y",
#                  expand = expansion(mult = c(0.01, 0.02))) +
#     labs(title = "Global Change-Point Centers (E.agglo, 26-D)",
#          x = "Year", y = NULL) +
#     theme_minimal(base_size = 12) +
#     theme(axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           panel.grid.major.y = element_blank(),
#           plot.title = element_text(hjust = 0.5))
#   print(p_centers)
#   ggsave("ecp_agglo_global_centers_square.png", p_centers,
#          width = 10, height = 10, units = "in", dpi = 300, bg = "white")
# }

# E.agglo默认不惩罚切点数量，此时算法会保留非常多的候选切点。