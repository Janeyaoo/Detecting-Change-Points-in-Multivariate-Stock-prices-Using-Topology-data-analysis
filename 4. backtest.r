# ===== TDA指标交易策略回测 =====
library(quantmod)
library(PerformanceAnalytics)
library(xts)

# ===== 1. 读取数据 =====
tda_l1 <- read.csv("/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/multi_tda1_data.csv")
tda_l2 <- read.csv("/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/multi_tda2_data.csv")

# ===== 2. 计算行均值作为整体TDA指标 =====
tda_l1_mean <- rowMeans(tda_l1[, !(names(tda_l1) %in% c("X", "date"))], na.rm = TRUE)
tda_l2_mean <- rowMeans(tda_l2[, !(names(tda_l2) %in% c("X", "date"))], na.rm = TRUE)

tda_l1_xts <- xts(tda_l1_mean, order.by = as.Date(tda_l1$date))
tda_l2_xts <- xts(tda_l2_mean, order.by = as.Date(tda_l2$date))

# ===== 3. 市场基准（S&P500） =====
getSymbols("^GSPC", from = "2010-01-01", to = "2023-12-31")
sp500_ret <- dailyReturn(Cl(GSPC))

# ===== 4. 生成交易信号 =====
# L1 信号
threshold_l1 <- quantile(tda_l1_xts, 0.95, na.rm = TRUE)
signal_l1 <- ifelse(tda_l1_xts > threshold_l1, 0, 1)
strategy_l1 <- sp500_ret * lag(signal_l1, 1)

# L2 信号
threshold_l2 <- quantile(tda_l2_xts, 0.95, na.rm = TRUE)
signal_l2 <- ifelse(tda_l2_xts > threshold_l2, 0, 1)
strategy_l2 <- sp500_ret * lag(signal_l2, 1)

# ===== 5. 对比回测结果 =====
png("tda_l1_l2_strategy.png", width = 12, height = 8, units = "in", res = 300)

# 设置整体绘图参数
par(cex.main = 1.6,   # 标题字体大小
    cex.lab  = 1.4,   # 坐标轴标题字体大小
    cex.axis = 1.2,   # 坐标轴刻度字体大小
    cex.legend = 1.4) # 图例字体大小

charts.PerformanceSummary(
  strategies,
  legend.loc = "topleft",
  main = "TDA-based Strategies vs Buy&Hold"
)

dev.off()