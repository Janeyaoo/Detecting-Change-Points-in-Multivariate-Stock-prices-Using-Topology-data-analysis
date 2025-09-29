###############################################################################
# 0) 依赖 & 工具函数
###############################################################################
suppressPackageStartupMessages({
  library(data.table)
})

# —— 列名选择：先精确，再标准化（小写/去空格/去标点）
pick_col <- function(dt, target_name) {
  stopifnot(is.data.table(dt))
  if (target_name %in% names(dt)) return(target_name)
  norm <- function(x) gsub("[[:punct:][:space:]]+", "", tolower(x))
  nm_norm <- norm(names(dt)); tgt_norm <- norm(target_name)
  hit <- which(nm_norm == tgt_norm)
  if (length(hit) == 1L) return(names(dt)[hit])
  if (length(target_name) > 1L) {
    for (cand in target_name) {
      if (cand %in% names(dt)) return(cand)
      cand_hit <- which(nm_norm == norm(cand))
      if (length(cand_hit) == 1L) return(names(dt)[cand_hit])
    }
  }
  stop(sprintf("找不到列 `%s`。实际列名：%s", target_name, paste(names(dt), collapse = ", ")))
}

# —— 日期→交易日索引
dates_to_index <- function(dates, calendar) {
  cal_idx <- setNames(seq_along(calendar), as.character(calendar))
  as.integer(cal_idx[as.character(as.Date(dates))])
}

# —— 将“各种形态”的预测结果统一为 Date 向量
# 支持：Date 向量 / 字符串日期向量 / 数值索引向量 / list$estimates / list$cp(s)
normalize_pred_dates <- function(x, market_dates) {
  n <- length(market_dates)
  to_dates <- function(idx) {
    idx <- as.integer(idx)
    idx <- idx[is.finite(idx) & idx >= 1L & idx <= n]
    # 去掉边界（常见做法：排除 1 和 n 以免把端点当变化点）
    idx <- idx[idx > 1L & idx < n]
    sort(unique(as.Date(market_dates[idx])))
  }
  if (inherits(x, "Date"))     return(sort(unique(x[x %in% market_dates])))
  if (is.character(x))         return(sort(unique(as.Date(x))))
  if (is.numeric(x))           return(to_dates(x))
  if (is.list(x)) {
    cand <- NULL
    if (!is.null(x$estimates)) cand <- x$estimates
    if (is.null(cand) && !is.null(x$cp))  cand <- x$cp
    if (is.null(cand) && !is.null(x$cps)) cand <- x$cps
    if (!is.null(cand)) return(if (is.numeric(cand)) to_dates(cand) else sort(unique(as.Date(cand))))
  }
  # 其他类型：返回空
  as.Date(character())
}

# —— 评估：在交易日历上做一对一贪心匹配（|Δ交易日| ≤ tol）
cp_eval <- function(truth_dates, pred_dates, market_dates, tol_trading_days = 21L) {
  cal   <- sort(unique(as.Date(market_dates)))
  truth <- sort(unique(as.Date(truth_dates))); truth <- truth[truth %in% cal]
  pred  <- sort(unique(as.Date(pred_dates)));  pred  <- pred [pred  %in% cal]

  t_idx <- dates_to_index(truth, cal)
  p_idx <- dates_to_index(pred,  cal)

  if (length(t_idx) == 0L || length(p_idx) == 0L) {
    TP <- 0L; FP <- length(p_idx); FN <- length(t_idx)
    precision <- ifelse(TP + FP > 0, TP/(TP+FP), NA_real_)
    recall    <- ifelse(TP + FN > 0, TP/(TP+FN), NA_real_)
    F1        <- ifelse(isTRUE(precision + recall > 0),
                        2*precision*recall/(precision+recall), NA_real_)
    return(list(TP=TP, FP=FP, FN=FN, precision=precision, recall=recall, F1=F1,
                matches=data.table()))
  }

  pairs <- CJ(t = t_idx, p = p_idx)
  pairs[, dist := abs(p - t)]
  pairs <- pairs[dist <= tol_trading_days]
  setorder(pairs, dist)

  matched_t <- rep(FALSE, length(t_idx))
  matched_p <- rep(FALSE, length(p_idx))
  out <- vector("list", 0L)

  if (nrow(pairs) > 0L) {
    for (i in seq_len(nrow(pairs))) {
      ti <- which(t_idx == pairs$t[i])
      pi <- which(p_idx == pairs$p[i])
      if (!matched_t[ti] && !matched_p[pi]) {
        matched_t[ti] <- TRUE; matched_p[pi] <- TRUE
        out[[length(out)+1L]] <- list(
          truth_date        = cal[t_idx[ti]],
          pred_date         = cal[p_idx[pi]],
          lag_trading_days  = p_idx[pi] - t_idx[ti],
          lag_calendar_days = as.integer(cal[p_idx[pi]] - cal[t_idx[ti]])
        )
      }
    }
  }

  TP <- sum(matched_t); FP <- length(p_idx) - TP; FN <- length(t_idx) - TP
  precision <- ifelse(TP + FP > 0, TP/(TP+FP), NA_real_)
  recall    <- ifelse(TP + FN > 0, TP/(TP+FN), NA_real_)
  F1        <- ifelse(isTRUE(precision + recall > 0),
                      2*precision*recall/(precision+recall), NA_real_)
  matches_dt <- if (length(out)) rbindlist(out) else data.table()
  list(TP=TP, FP=FP, FN=FN, precision=precision, recall=recall, F1=F1, matches=matches_dt)
}

# —— 从 global_* 提取预测（用 center_median）
extract_pred_from_global <- function(global_obj) {
  stopifnot(is.list(global_obj), !is.null(global_obj$global_events))
  ge <- as.data.table(global_obj$global_events)
  stopifnot("center_median" %in% names(ge))
  sort(unique(as.Date(ge$center_median[!is.na(ge$center_median)])))
}

# —— TDA 超阈值 → 变点（默认 98% 分位；压缩近邻 min_sep 个交易日）
tda_detect <- function(dt_tda, value_col, date_col, market_dates, prob = 0.91, min_sep = 21L) {
  x <- dt_tda[[value_col]]
  thr <- as.numeric(quantile(x, probs = prob, na.rm = TRUE))
  idx <- which(!is.na(x) & x >= thr)
  if (length(idx) == 0L) return(as.Date(character()))
  dates <- as.Date(dt_tda[[date_col]])
  pos <- dates_to_index(dates[idx], market_dates)
  pos <- sort(unique(na.omit(pos)))
  if (length(pos) == 0L) return(as.Date(character()))
  keep <- c(TRUE, diff(pos) > min_sep)
  pos <- pos[keep]
  market_dates[pos]
}

###############################################################################
# 1) 交易日历（market_dates）
###############################################################################
csv_cal <- "/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/multi_log_return_data.csv"
dt_cal  <- fread(csv_cal)
#print(dt_cal)
date_col <- intersect(tolower(names(dt_cal)), c("date"))
stopifnot(length(date_col) == 1L)
dt_cal[, Date := as.Date(get(date_col))]
market_dates <- sort(unique(na.omit(dt_cal$Date)))

###############################################################################
# 2) Ground truth（真集）与容忍窗口
###############################################################################
truth_cp <- as.Date(c(
  "2011-04-18",
  "2011-05-06",
  "2011-06-06",
  "2011-08-05",
  "2011-08-08",
  "2012-07-06",
  "2013-05-22",
  "2015-01-15",
  "2015-08-24",
  "2016-06-24",
  "2016-11-02",
  "2016-12-06",
  "2018-10-10",
  "2020-02-24",
  "2020-03-23",
  "2020-04-20",
  "2020-06-25",
  "2021-09-23",
  "2021-11-26",
  "2021-12-15",
  "2022-02-24",
  "2022-06-13",
  "2022-09-28",
  "2022-10-13",
  "2023-03-10",
  "2023-06-16",
  "2023-07-24",
  "2023-10-27",
  "2023-11-14"
))
tol_trading_days <- 21L

###############################################################################
# 3) 三种全局方法（Rolling / PELT / HMM）
#    假设 global_roll / global_pelt / global_hmm 已存在（含 $global_events$center_median）
###############################################################################
if (!exists("global_roll")) warning("global_roll 不存在：请在此之前生成该对象")
if (!exists("global_pelt")) warning("global_pelt 不存在：请在此之前生成该对象")
if (!exists("global_hmm"))  warning("global_hmm 不存在：请在此之前生成该对象")

pred_roll <- if (exists("global_roll")) extract_pred_from_global(global_roll) else as.Date(character())
pred_pelt <- if (exists("global_pelt")) extract_pred_from_global(global_pelt) else as.Date(character())
pred_hmm  <- if (exists("global_hmm"))  extract_pred_from_global(global_hmm)  else as.Date(character())

###############################################################################
# 4) TDA 指标（L1/L2）→ 超过 98% 分位点视为变点（含最小间隔去重）
###############################################################################
dt_tda <- fread("/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/Tda_indexs.csv")
nm <- names(dt_tda)
# 列名标准化：小写、去空格、去非字母数字（下划线等也去掉）
nm_norm <- tolower(trimws(gsub("[^a-z0-9]+", "", nm)))
pick_col <- function(key) {
  key_norm <- tolower(trimws(gsub("[^a-z0-9]+", "", key)))
  # 先精确匹配
  idx_exact <- which(nm_norm == key_norm)
  if (length(idx_exact) == 1L) return(nm[idx_exact])
  if (length(idx_exact) > 1L)  return(nm[idx_exact[which.min(nchar(nm[idx_exact]))]])
  # 再前缀匹配（兜底）
  idx_pref <- grep(paste0("^", key_norm), nm_norm)
  if (length(idx_pref) >= 1L) return(nm[idx_pref[1L]])
  stop(sprintf("找不到列 `%s`。实际列名：%s", key, paste(nm, collapse = ", ")))
}
# 现在以下写法都能正常工作：
date_col_tda <- pick_col("date")        # 或 "Date"
L1_col       <- pick_col("L1_mean")     # 或 "l1mean" / "l1 mean"
L2_col       <- pick_col("L2_mean")     # 或 "l2"

# 如果需要把 <IDat> 转为 Date 列（可选）
dt_tda[, Date := as.Date(get(date_col_tda))]
setorder(dt_tda, Date)

pred_tda_l1 <- tda_detect(dt_tda, value_col = L1_col, date_col = date_col_tda,
                          market_dates = market_dates, prob = 0.91, min_sep = 10L)
pred_tda_l2 <- tda_detect(dt_tda, value_col = L2_col, date_col = date_col_tda,
                          market_dates = market_dates, prob = 0.91, min_sep = 10L)
print(pred_tda_l1)
print(pred_tda_l2)
###############################################################################
# 5) ecp 方法（E.agglo / E.divisive）
###############################################################################
normalize_pred_dates <- function(obj, market_dates) {
  # 直接是 Date / 字符日期
  if (inherits(obj, "Date"))   return(sort(unique(as.Date(obj))))
  if (is.character(obj))       return(sort(unique(as.Date(obj))))

  # 直接是索引
  if (is.numeric(obj) || is.integer(obj)) {
    idx <- as.integer(obj)
    idx <- idx[idx >= 1L & idx <= length(market_dates)]
    return(sort(unique(as.Date(market_dates[idx]))))
  }

  # list / ecp 对象各种字段兜底
  if (is.list(obj)) {
    # 你自定义的 detect_* 返回
    if (!is.null(obj$cp_dates))          return(sort(unique(as.Date(obj$cp_dates))))
    if (!is.null(obj$global_events) &&
        "center_median" %in% names(obj$global_events)) {
      return(sort(unique(as.Date(obj$global_events$center_median))))
    }
    # ecp::e.agglo / e.divisive 返回（通常有 estimates）
    if (!is.null(obj$estimates))         return(normalize_pred_dates(obj$estimates, market_dates))
    if (!is.null(obj$cp))                return(normalize_pred_dates(obj$cp,        market_dates))
    if (!is.null(obj$cps))               return(normalize_pred_dates(obj$cps,       market_dates))
  }

  # 实在不认识 → 空
  as.Date(character())
}

# 传入一组“对象名”，返回第一个找到的并规范化为 Date
get_pred <- function(obj_names, market_dates) {
  for (nm in obj_names) {
    if (exists(nm, inherits = TRUE)) {
      obj <- get(nm, inherits = TRUE)
      return(normalize_pred_dates(obj, market_dates))
    }
  }
  as.Date(character())
}

# 现在把你真正的对象也纳入候选名里（注意包含 ecp_agglo / ecp_div）
pred_agglo <- get_pred(c("ecp_agglo", "agglo_res", "agglo_idx", "pred_agglo"), market_dates)
pred_div   <- get_pred(c("ecp_div",   "div_res",   "div_idx",   "pred_div"),   market_dates)
###############################################################################
# 6) 评估：Precision / Recall / F1（七种方法）
###############################################################################
res_roll   <- cp_eval(truth_cp, pred_roll,   market_dates, tol_trading_days)
res_pelt   <- cp_eval(truth_cp, pred_pelt,   market_dates, tol_trading_days)
res_hmm    <- cp_eval(truth_cp, pred_hmm,    market_dates, tol_trading_days)
res_tda_l1 <- cp_eval(truth_cp, pred_tda_l1, market_dates, tol_trading_days)
res_tda_l2 <- cp_eval(truth_cp, pred_tda_l2, market_dates, tol_trading_days)
res_agglo  <- cp_eval(truth_cp, pred_agglo,  market_dates, tol_trading_days)
res_div    <- cp_eval(truth_cp, pred_div,    market_dates, tol_trading_days)

method_summary <- data.table(
  Method    = c("Rolling Std", "PELT (MBIC)", "HMM (Viterbi)",
                "TDA L1 (91%)", "TDA L2 (91%)", "E.agglo", "E.divisive"),
  Precision = c(res_roll$precision, res_pelt$precision, res_hmm$precision,
                res_tda_l1$precision, res_tda_l2$precision, res_agglo$precision, res_div$precision),
  Recall    = c(res_roll$recall, res_pelt$recall, res_hmm$recall,
                res_tda_l1$recall, res_tda_l2$recall, res_agglo$recall, res_div$recall),
  F1        = c(res_roll$F1, res_pelt$F1, res_hmm$F1,
                res_tda_l1$F1, res_tda_l2$F1, res_agglo$F1, res_div$F1),
  TP        = c(res_roll$TP, res_pelt$TP, res_hmm$TP,
                res_tda_l1$TP, res_tda_l2$TP, res_agglo$TP, res_div$TP),
  FP        = c(res_roll$FP, res_pelt$FP, res_hmm$FP,
                res_tda_l1$FP, res_tda_l2$FP, res_agglo$FP, res_div$FP),
  FN        = c(res_roll$FN, res_pelt$FN, res_hmm$FN,
                res_tda_l1$FN, res_tda_l2$FN, res_agglo$FN, res_div$FN)
)


# 如需查看各方法的配对与滞后（交易日/日历天），可单独查看：
# res_roll$matches; res_pelt$matches; res_hmm$matches;
# res_tda_l1$matches; res_tda_l2$matches; res_agglo$matches; res_div$matches
print(method_summary)

###############################################################################
# 7) Visualization (MDPI style) ######
###############################################################################
library(data.table)
library(ggplot2)

# ===== 输入 F1 值 =====
dt <- data.table(
  Method = c("Rolling Std","PELT (MBIC)","HMM (Viterbi)",
             "E.agglo","E.divisive","TDA L1","TDA L2"),
  # `90%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.5556, 0.5769),
  # `92%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.4255, 0.4348),
  # `95%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.2632, 0.2051),
  # `98%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.1765, 0.1765),
  # `99%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.0625, 0.0625),

  `92%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.5556, 0.5769),
  `95%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.5200, 0.6000),
  `98%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.4255, 0.4348),
  `99%` = c(0.0645, 0.2174, 0.4096, 0.2703, 0.2353, 0.2632, 0.2051)
)

# 宽转长
dt_long <- melt(dt, id.vars = "Method", variable.name = "Threshold", value.name = "F1")

# ===== 设置方法顺序和配色 =====
methods_order <- c("Rolling Std","PELT (MBIC)","E.divisive",
                   "E.agglo","HMM (Viterbi)","TDA L1","TDA L2")
dt_long[, Method := factor(Method, levels = methods_order)]

base_raw <- c(
  "#E69F00", # Rolling Std (orange)
  "#56B4E9", # PELT (sky blue)
  "#009E73", # E.divisive (bluish green)
  "#F0E442", # E.agglo (yellow)
  "#0072B2", # HMM (bluish green blue)
  "#D55E00", # TDA L1 (vermillion)
  "#CC79A7"  # TDA L2 (reddish purple)
)
names(base_raw) <- methods_order

# ===== 绘制图形 =====
p_f1 <-
  ggplot(dt_long, aes(x = Threshold, y = F1, fill = Method)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    labs(title = "Comparison of CPD methods under different thresholds",
        x = "Threshold", y = "F1") +
    scale_fill_manual(values = base_raw) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )

# 保存图片
ggsave("f1_threshold_comparison_colored.png", p_f1,
       width = 9, height = 5, dpi = 300, bg = "white")
