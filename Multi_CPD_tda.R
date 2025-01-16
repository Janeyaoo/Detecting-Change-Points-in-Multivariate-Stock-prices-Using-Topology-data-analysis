
##### 阈值标注实验 #####
df <- normalized_tda_minmax 

# 指定需要计算均值的列范围，例如前 3 列
columns_to_average <- 3:28

# 添加新列，计算前几列的均值
df$mean_tda <- rowMeans(df[, columns_to_average])

# 查看结果
print(df)

install.packages("entropy")
library(entropy)

# 计算每个时间点的熵
overall_fluctuation <- apply(normalized_tda_minmax[,3:28], 1, function(x) entropy::entropy(discretize(x)))

# 查看结果

# 加载库
library(entropy)
# 自定义离散化函数
discretize <- function(x, bins = 5) {
  cut(x, breaks = bins, labels = FALSE)
}

# 计算每行的熵值
overall_fluctuation <- apply(normalized_tda_minmax[,3:28], 1, function(x) {
  discrete_x <- discretize(x, bins = 5)               # 离散化
  freq <- table(discrete_x) / length(discrete_x)      # 频率分布
  entropy::entropy(freq)                              # 计算熵
})

normalized_tda_minmax$entropy <- overall_fluctuation

# 示例数据：生成一个时间序列
time_series <- df$mean_tda  # 长度为1000的随机序列

# 计算显著性阈值
alpha <- 0.01                              # 显著性水平
threshold <- quantile(time_series, probs = 1 - alpha)  # 取前 1%

# 提取显著点的位置和值
significant_points <- which(time_series > threshold)   # 超过阈值的索引
significant_values <- time_series[significant_points]  # 对应的值

# 绘制时间序列
plot(time_series, type = "l", col = "blue", lwd = 2,
     main = "Time Series with Significant Points",
     xlab = "Time", ylab = "Value")

# 标注显著性点
points(significant_points, significant_values, col = "red", pch = 16)  # 红点标注
text(significant_points, significant_values, 
     labels = round(significant_values, 2), 
     pos = 3, col = "red", cex = 0.8)  # 显示值

# 添加阈值线
abline(h = threshold, col = "green", lty = 2, lwd = 2)  # 阈值线

# 图例
legend("topright", legend = c("Time Series", "Significant Points", "Threshold"),
       col = c("blue", "red", "green"), lty = c(1, NA, 2), 
       pch = c(NA, 16, NA), lwd = c(2, NA, 2))


##### 绘制Figure2.1 #####
library(tidyr)
library(ggplot2)
# 查看数据框结构

figure_data <- data.frame(
  time <- multi_stock_data$date,
  stock <- apply(multi_stock_data[3:28],1,mean),
  L1 <- apply(normalized_tda1_minmax[,3:28],1,mean),
  L2 <- apply(normalized_tda2_minmax[,3:28],1,mean)
)
colnames(figure_data) <- c("time","stock", "L1", "L2")

str(figure_data)
head(figure_data)

p <- ggplot(figure_data, aes(x = time)) +
  geom_line(aes(y = stock), color = "#990307", size = 1, linetype = "solid") +  # 第一条曲线
  labs(
    title = "Time Series Plot",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 添加其他时间序列
p <- p +
  geom_line(aes(y = L1*100), color = "#176B2D", size = 1, linetype = "solid") +    # 第二条曲线
  geom_line(aes(y = L2*100), color = "#286ABA", size = 1, linetype = "solid")  # 第三条曲线

# 打印图像
print(p)
ggsave("time_series_plot.png", width = 12, height = 6, dpi = 300)

##### 绘制Figure2.2 #####
figure22 <- data.frame(
  figure_data <- data.frame(
    time <- multi_stock_data$date,
    L1 <- apply(normalized_tda1_minmax[,3:28],1,mean),
    L2 <- apply(normalized_tda2_minmax[,3:28],1,mean)
  )
)
write.csv(figure22, "/Users/yaojian/tda_ph_timeserise/data/December_analysis/figure22.csv", row.names = FALSE)
colnames(figure22) <- c("time", "L1", "L2")
library(ggplot2)

# 确保时间是日期格式
figure22$time <- as.Date(figure22$time)

# 绘制图像
ggplot(data = figure22, aes(x = time)) +
  # 绘制 L1 曲线
  geom_line(aes(y = L1), color = "blue", size = 1, linetype = "solid") +
  # 绘制 L2 曲线
  geom_line(aes(y = L2), color = "red", size = 1, linetype = "dashed") +
  # 添加 0.01 显著水平线
  geom_hline(yintercept = 0.99, color = "green", linetype = "dotted", size = 1) +
  # 图像标题和标签
  labs(
    title = "L1 and L2 Time Series with 0.01 Significance Level",
    x = "Time",
    y = "Value",
    color = "Legend"
  ) +
  # 简洁主题
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )
ggsave("time_series_l1_l2.png", width = 12, height = 6, dpi = 300)

library(ggplot2)

# 确保时间是日期格式
figure22$time <- as.Date(figure22$time)

# 计算 L1 和 L2 的 0.01 阈值
L1_threshold <- sort(figure22$L1, decreasing = TRUE)[ceiling(0.02 * nrow(figure22))]
L2_threshold <- sort(figure22$L2, decreasing = TRUE)[ceiling(0.02 * nrow(figure22))]

# 绘制 L1 时间序列图
plot_L1 <- ggplot(figure22, aes(x = time)) +
  geom_line(aes(y = L1), color = "#176B2D", size = 1) +  # 绘制 L1 曲线
  geom_hline(yintercept = L1_threshold, color = "red", linetype = "dashed", size = 1) +  # 添加 L1 阈值线
  labs(
    title = "L1 Time Series with 0.02 Threshold",
    x = "Time",
    y = "L1 Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 绘制 L2 时间序列图
plot_L2 <- ggplot(figure22, aes(x = time)) +
  geom_line(aes(y = L2), color = "#286ABA", size = 1) +  # 绘制 L2 曲线
  geom_hline(yintercept = L2_threshold, color = "red", linetype = "dashed", size = 1) +  # 添加 L2 阈值线
  labs(
    title = "L2 Time Series with 0.02 Threshold",
    x = "Time",
    y = "L2 Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 打印图像
print(plot_L1)
ggsave("time_series_l1.png", width = 12, height = 6, dpi = 300)

print(plot_L2)
ggsave("time_series_l2.png", width = 12, height = 6, dpi = 300)

library(patchwork)
combined_plot <- plot_L1 / plot_L2  # 上下排列
print(combined_plot)
ggsave("time_series_l12.png", width = 12, height = 6, dpi = 300)

##### 绘制figure2.3 event1 #####
# 将2021年对应的时间的数据放大,确定显著范围后，前后各扩大一个月以绘图观察趋势！
library(ggplot2)
figure_data <- data.frame(
  time <- multi_stock_data$date[],
  stock <- apply(multi_stock_data[,3:28],1,mean),
  L1 <- apply(normalized_tda1_minmax[,3:28],1,mean),
  L2 <- apply(normalized_tda2_minmax[,3:28],1,mean)
)
colnames(figure_data) <- c("time","stock", "L1", "L2")


event2021 <- data.frame(
  time <- figure_data$time[2451:2552],
  stock <- figure_data$stock[2451:2552],
  L1 <- figure_data$L1[2451:2552],
  L2 <- figure_data$L2[2451:2552]
)

str(figure_data)
head(figure_data)

p <- ggplot(event2021, aes(x = time)) +
  geom_line(aes(y = stock), color = "#990307", size = 1, linetype = "solid") +  # 第一条曲线
  labs(
    title = "Change Point 2021",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 添加其他时间序列
p <- p +
  geom_line(aes(y = L1*10), color = "#176B2D", size = 1, linetype = "solid") +    # 第二条曲线
  geom_line(aes(y = L2*10), color = "#286ABA", size = 1, linetype = "solid")  # 第三条曲线

# 打印图像
print(p)
ggsave("event2021.png", width = 6, height = 6, dpi = 300)
dev.off()

##### 绘制figure2.3 event2 #####
# 将2012年对应的时间的数据放大,确定显著范围后，前后各扩大一个月以绘图观察趋势！
event2012 <- data.frame(
  time <- figure_data$time[48:133],
  stock <- figure_data$stock[48:133],
  L1 <- figure_data$L1[48:133],
  L2 <- figure_data$L2[48:133]
)

str(figure_data)
head(figure_data)

p <- ggplot(event2012, aes(x = time)) +
  geom_line(aes(y = stock), color = "#990307", size = 1, linetype = "solid") +  # 第一条曲线
  labs(
    title = "Change point 2012",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 添加其他时间序列
p <- p +
  geom_line(aes(y = L1*70), color = "#176B2D", size = 1, linetype = "solid") +    # 第二条曲线
  geom_line(aes(y = L2*70), color = "#286ABA", size = 1, linetype = "solid")  # 第三条曲线

# 打印图像
print(p)
ggsave("event2012.png", width = 6, height = 6, dpi = 300)
dev.off()

##### 绘制figure2.3 event3 #####
# 将2016年对应的时间的数据放大,确定显著范围后，前后各扩大一个月以绘图观察趋势！

event2016 <- data.frame(
  time <- figure_data$time[1407:1517],
  stock <- figure_data$stock[1407:1517],
  L1 <- figure_data$L1[1407:1517],
  L2 <- figure_data$L2[1407:1517]
)

str(figure_data)
head(figure_data)

p <- ggplot(event2016, aes(x = time)) +
  geom_line(aes(y = stock), color = "#990307", size = 1, linetype = "solid") +  # 第一条曲线
  labs(
    title = "Change point 2016",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 添加其他时间序列
p <- p +
  geom_line(aes(y = L1*100), color = "#176B2D", size = 1, linetype = "solid") +    # 第二条曲线
  geom_line(aes(y = L2*100), color = "#286ABA", size = 1, linetype = "solid")  # 第三条曲线

# 打印图像
print(p)
ggsave("event2016.png", width = 6, height = 6, dpi = 300)
dev.off()

##### 绘制figure2.3 event4 #####
# 将2023年对应的时间的数据放大,确定显著范围后，前后各扩大一个月以绘图观察趋势！
event2023 <- data.frame(
  time <- figure_data$time[3021:3133],
  stock <- figure_data$stock[3021:3133],
  L1 <- figure_data$L1[3021:3133],
  L2 <- figure_data$L2[3021:3133]
)

str(figure_data)
head(figure_data)

p <- ggplot(event2023, aes(x = time)) +
  geom_line(aes(y = stock), color = "#990307", size = 1, linetype = "solid") +  # 第一条曲线
  labs(
    title = "Change point 2023",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14)
  )

# 添加其他时间序列
p <- p +
  geom_line(aes(y = L1*100), color = "#176B2D", size = 1, linetype = "solid") +    # 第二条曲线
  geom_line(aes(y = L2*100), color = "#286ABA", size = 1, linetype = "solid")  # 第三条曲线

# 打印图像
print(p)
ggsave("event2023.png", width = 6, height = 6, dpi = 300)
dev.off()
##### 分sector计算norm ######
# 计算每列均值
column_means <- apply(multi_tda2_data[,3:28], 2, mean, na.rm = TRUE)

print(column_means)

tech_1_mean <- mean(column_means["bidu"],column_means["crm"],column_means["orcl"],column_means["qcom"],column_means["TSM"])
print(tech_1_mean)

industry_1_mean <- mean(column_means["aal"],column_means["tm"])
print(industry_1_mean)

Consumer_1_mean <- mean(column_means["cmg"],column_means["cost"],column_means["ebay"],column_means["KO"],column_means["nke"],column_means["pep"] )
print(Consumer_1_mean)

Finance_1_mean <- mean(column_means["BRK-B"],column_means["v"],column_means["xlf"],column_means["gld"])
print(Finance_1_mean)

Energy_1_mean <- mean(column_means["cop"],column_means["uso"],column_means["bhp"])
print(Energy_1_mean)

health_1_mean <- mean(column_means["biib"],column_means["gild"],column_means["amgn"],column_means["gsk"],column_means["mrk"])
print(health_1_mean)

Tele_1_mean <- mean(column_means["cmcsa"])
print(Tele_1_mean)


tech_2_mean <- mean(column_means["bidu"],column_means["crm"],column_means["orcl"],column_means["qcom"],column_means["TSM"])
print(tech_2_mean)

industry_2_mean <- mean(column_means["aal"],column_means["tm"])
print(industry_2_mean)

Consumer_2_mean <- mean(column_means["cmg"],column_means["cost"],column_means["ebay"],column_means["KO"],column_means["nke"],column_means["pep"] )
print(Consumer_2_mean)

Finance_2_mean <- mean(column_means["BRK-B"],column_means["v"],column_means["xlf"],column_means["gld"])
print(Finance_2_mean)

Energy_2_mean <- mean(column_means["cop"],column_means["uso"],column_means["bhp"])
print(Energy_2_mean)

health_2_mean <- mean(column_means["biib"],column_means["gild"],column_means["amgn"],column_means["gsk"],column_means["mrk"])
print(health_2_mean)

Tele_2_mean <- mean(column_means["cmcsa"])
print(Tele_2_mean)




##### 分area计算norm ######
# 计算每列均值
column_means <- apply(multi_tda1_data[,3:28], 2, mean, na.rm = TRUE)

print(column_means)

usa_1_mean <- mean(column_means["crm"],column_means["orcl"],column_means["qcom"],column_means["aal"],column_means["cmg"],column_means["cost"],column_means["ebay"],column_means["KO"],column_means["nke"],column_means["pep"],column_means["BRK-B"],column_means["v"],column_means["xlf"],column_means["gld"],column_means["cop"],
                   column_means["uso"],column_means["amgn"],column_means["biib"],column_means["gild"],column_means["gsk"],column_means["mrk"],column_means["cmcsa"])
print(usa_1_mean)

china_1_mean <- mean(column_means["bidu"],column_means["TSM"])
print(china_1_mean)

australia_1_mean <- mean(column_means["bhp"])
print(australia_1_mean)

japan_1_mean <- mean(column_means["tm"])
print(japan_1_mean)



