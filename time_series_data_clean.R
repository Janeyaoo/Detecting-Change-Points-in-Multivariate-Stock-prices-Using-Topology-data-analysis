 install.packages("dplyr")
 install.packages("tidyr")
 install.packages("lubridate")
 
library(dplyr)
library(tidyr) 
library(lubridate) 
library(base)
 
##### 1. 将全部50支股票数据按照时间对齐 #####
FUN <- function(file){
  data <- read.csv(file)
  return(data)
}
file_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
file_list <- list.files(file_path, recursive = TRUE,full.names = TRUE)
data_list <- lapply(file_list, FUN)

# 给每个数据框一个名字
names(data_list) <- paste0("Stock_", 1:50)
# 对所有股票数据进行时间对齐
aligned_data <- data_list %>%
  # 将所有数据框进行绑定
  bind_rows(.id = "stock") %>%
  # 将数据整理成宽格式
  pivot_wider(names_from = "stock", values_from = "Close") %>%
  # 按照日期排列
  arrange(Date)

# 填补NA值，如果需要，可以选择用向前填充、插值等方法
# 这里我们使用向前填充
aligned_data_filled <- aligned_data %>%
  mutate(across(starts_with("Stock"), ~ zoo::na.locf(., na.rm = FALSE)))

# 打印结果
print(aligned_data_filled)

##### 2. 将50支股票的数据取出时间交集的部分 （结果无交集）##### 
library(dplyr)

# 假设 data_list 中的每个数据框都有一列名为 "Date" 的日期或时间戳
# 首先，收集所有数据框的日期
all_dates <- lapply(data_list, function(df) df$Date)

# 找到所有日期中的交集
common_dates <- Reduce(intersect, all_dates)

# 使用交集日期来筛选每个数据框，使其只保留共有的日期
data_list_common <- lapply(data_list, function(df) {
  df %>% filter(Date %in% common_dates)
})
# 检查结果
str(data_list_common)

##### 3.输出数据文件夹中各个数据的行维度 
file_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
file_list <- list.files(file_path, recursive = TRUE,full.names = TRUE)
nrow_length <- list()
print(length(file_list))

for (i in 1:50) {
  nrow_length[i] <- nrow(read.csv(file_list[i]))
}

# 获取列表中的值
values <- unlist(nrow_length)

# 获取排序的索引
sorted_indices <- order(values)

print(sorted_indices)

# 根据排序的索引对列表进行排序
sorted_list <- nrow_length[sorted_indices]

sorted_time_stock <- data.frame(
  ID <- sorted_indices,
  Obeservations <- sorted_list 
)
# 3.按照观测量排序发现其中有26支股票大致有将近3400值，因此将这些挑选出来 可以尽量补充缺失值吧？ 但是时间段没对齐耶 ####

time_span_matrix <- matrix(NA, nrow = 50, ncol = 4)
print(time_span_matrix[1])
library(tools)
printsubstr((basename(file_list[1])))

data_name <- file_path_sans_ext(basename(file_list[1]))
result <- substr(data_name, 10, nchar(data_name))

for (i in 1:50) {
  data <-read.csv(file_list[i])
  began_time <- data$Date[1]
  end_time <- data$Date[nrow(data)]
  time_span_matrix[i,1] <- substr(file_path_sans_ext(basename(file_list[i])),10,nchar(file_path_sans_ext(basename(file_list[1]))))
  time_span_matrix[i,2] <- substr(began_time,1,10)
  time_span_matrix[i,3] <- substr(end_time,1,10)
  time_span_matrix[i,4] <- nrow(data)
}
write.csv(time_span_matrix, file = "/Users/yaojian/tda_ph_timeserise/data/stock_time_matrix_with_names.csv", row.names = TRUE)
# 导出50只股票时间跨度和观测量大表格

##### 数据平稳化操作
# 差分处理

# 对数变换

##### 4.对数据进行去趋势Detrending的处理  ####
# 4.1 
library(zoo)  # zoo包用于计算滚动平均
xlf_stock_data <- read.csv(file_list[50])
xlf_data <- data.frame(Date = substr(xlf_stock_data$Date,1,10), Close_price = xlf_stock_data$Close)
# 对数变换
xlf_data$Log_Price <- log(xlf_data$Close_price)

# 计算移动平均（例如，使用 10 日移动平均）
window_size <- 10 # 低频日度数据一般用10天就可以
xlf_data$MA <- rollmean(xlf_data$Log_Price, window_size, fill = NA, align = "center")
# 将结果绘制出来
library(ggplot2)
# 确保 xlf_data$Date 是日期类型，如果不是，可以转换它
xlf_data$Date <- as.Date(xlf_data$Date)
# 绘制收盘价和 10 日移动平均
ggplot(xlf_data, aes(x = Date)) +
  geom_line(aes(y = Log_Price), color = "black", size = 0.5) +
  geom_line(aes(y = MA), color = "blue", size = 0.5) +
  labs(title = "Close Price and 100-Day Moving Average", x = "Date", y = "Price") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")

# 感觉用100的窗口进行移动平均会能够更明显地看出波动

# 4.2 去季节性：
# 4.2.1 stl() 适用于周期性变化明显的时间序列数据，支持LOESS平滑
library(forecast)
xlf_data_NA <- xlf_data[!is.na(xlf_data$MA) & !is.na(xlf_data$Date), ]
ts_data <- ts(xlf_data_NA$MA, frequency = 12,)  # 例如，数据按月（月度频率）
# 使用 stl() 分解时间序列
decomposed_ts <- stl(ts_data, s.window = "periodic")
# 绘制分解结果
plot(decomposed_ts)
# 提取趋势和季节性成分
trend_component <- decomposed_ts$time.series[, "trend"]
seasonal_component <- decomposed_ts$time.series[, "seasonal"]
# 去除季节性：从原始数据中减去季节性成分
deseasonalized_data <- ts_data - seasonal_component
# 查看去季节性后的数据
plot(deseasonalized_data, main = "Deseasonalized Data", col = "blue")

# 4.2.2 使用 decompose() 函数进行季节性分解
ts_data <- ts(xlf_data$Close_price, frequency = 12)  # 假设月度数据
decomposed_ts <- decompose(ts_data)
# 绘制分解结果
plot(decomposed_ts)
# 提取季节性成分
seasonal_component <- decomposed_ts$seasonal
# 去除季节性：从原始数据中减去季节性成分
deseasonalized_data <- ts_data - seasonal_component
# 绘制去季节性后的数据
plot(deseasonalized_data, main = "Deseasonalized Data", col = "blue")

# 4.2.3 可以帮助进行季节性检验，适用于确定数据中是否存在季节性。
install.packages("seastests")
library(seastests)
# 使用 seastests 包中的季节性检验函数
result <- qs(ts(xlf_data$Close_price, freq = 12))
# 打印测试结果
print(result)

# 4.2.4 
# 使用 auto.arima() 自动选择季节性模型
library(forecast)
# 假设时间序列数据 ts_data
fit <- auto.arima(xlf_data$Log_Price, seasonal = TRUE)
# 使用模型进行季节性调整
deseasonalized_data <- xlf_data$Log_Price - fitted(fit)
# 绘制去季节性后的数据
plot(deseasonalized_data, main = "Deseasonalized Data Using ARIMA", col = "black")
print(fitted(fit))

# 3. 平稳性检测和处理
# 单位根检验 (ADF)
library(tseries)
adf_test <- adf.test(deseasonalized_data)
print(adf_test)
if(adf_test$p.value > 0.05) {
  # 若数据不平稳，进行差分处理
  log_returns <- diff(log_returns)
}

#### 经过以上思考
log_stock <- list()
clean_stock <- list()
for(i in 1:50){
  data <- read.csv(file_list[i])[5]
  Log_data <- log(data)
  log_stock[i] <- Log_data
  fit <- auto.arima(Log_data, seasonal = TRUE)
  # 使用模型进行季节性调整
  fit_data <- data.frame(fitted(fit))
  arima_data <- Log_data - fit_data
  clean_stock[i] <- arima_data
  }
# 保存为 RData 文件
save(log_stock, file = "/Users/yaojian/tda_ph_timeserise/data/log_close_price.RData")
save(clean_stock, file = "/Users/yaojian/tda_ph_timeserise/data/clean_close_price.RData")

# 平稳性测试
adf_result <- list()

for(i in 1:50){
  adf_input <- unlist(clean_stock[[i]])
  adf_test <- adf.test(adf_input)
  adf_result[i] <- adf_test$p.value
}
##### 测试结果显示这时的clean数据都是很平稳的 我们在这个平稳的数据上进行计算

##### 5.将原始的数据按照国家展示出来#####

install.packages("ggplot2")
library(xts)
library(ggplot2)
library(dplyr)
library(tidyr)
### 中国的三家 close股价数据
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

Bidu_data <- read.csv(modified_data_list[9])
Bidu_data_frame <- data.frame(
  Date <- Bidu_data$Date,
  Bidu_close_price <- Bidu_data[5]
  )

colnames(Bidu_data_frame) <- c("Date","Bidu_close_price")
 
BABA_data <- read.csv(modified_data_list[7])
BABA_data_frame <- data.frame(
  Date <- BABA_data$Date,
  BABA_close_price <- BABA_data[5]
)
colnames(BABA_data_frame) <- c("Date","BABA_close_price")

TSM_data <- read.csv(modified_data_list[45])
TSM_data_frame <- data.frame(
  Date <- TSM_data$Date,
  TSM_close_price <- TSM_data[6]
)
colnames(TSM_data_frame) <- c("Date","TSM_close_price")
# 查看三个时间序列
head(Bidu_data_frame)
head(BABA_data_frame)
head(TSM_data_frame)


# 将三个时间序列数据合并在一起，对于没有数据的时间段，自动填充为NA
China_merged <- Bidu_data_frame %>%
  full_join(BABA_data_frame, by = "Date") %>%
  full_join(TSM_data_frame, by = "Date")

# 查看合并后的数据
head(China_merged)

Plot_China <- data.frame(
  Timestamp <- as.POSIXct(China_merged$Date),
  Bidu <- China_merged$Bidu_close_price,
  BABA <- China_merged$BABA_close_price,
  TSM <- China_merged$TSM_close_price,
  Mean <- mean(Bidu,BABA,TSM)
)

ggplot(China_merged, aes(x = Timestamp)) + 
  geom_line(aes(y = Bidu),color = "black", linewidth = 0.2) +  # 控制线条宽度
  geom_line(aes(y = BABA),color = "#A6519E", linewidth = 0.2) +
  geom_line(aes(y = TSM),color = "#68BD48", linewidth = 0.2) +
  
  scale_x_datetime(date_labels = "%Y-%m-%d") +  # 使用 scale_x_datetime 处理日期
  # 时间数据处理：如果你处理的是日期时间数据，确保正确使用日期相关的标度函数，例如 scale_x_date()、scale_y_date() 或 scale_x_datetime()。
  scale_y_continuous()+   #确保y是连续型
  # 减小点的大小并设置透明度
  labs(title = "China Merged data", x = "Date", y = "Value") 
theme_minimal()



#####6. 求 daliy return ####
log_return <- list()
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
for (i in 1:50) {
  data <- read.csv(modified_data_list[i])
  close_price_frame <- data.frame(Date <- data$Date, Close <- data$Close)
  close_price_frame$log_return <- log(close_price_frame$Close / c(NA, close_price_frame$Close[-nrow(close_price_frame)]))
  log_return[[i]] <-  close_price_frame$log_return
}
save(log_return, file = "/Users/yaojian/tda_ph_timeserise/data/Nov_analysis/log_return_price.RData")


