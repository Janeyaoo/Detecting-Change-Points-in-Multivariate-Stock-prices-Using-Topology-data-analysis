
##### 1. 读取并清洗原始数据 #####
stock_data <- read.csv("/Users/jane/Desktop/PH/tda_ph_timeserise/data/December_analysis/multi_stock_data0925.csv")
head(stock_data)
str(stock_data)
summary(stock_data)

# 假设 stock_data 已经在环境中
biib_data <- data.frame(
  date = stock_data$date,
  biib = stock_data$biib
)
head(biib_data)
biib_data$date <- as.Date(biib_data$date)
biib_data$return_log <- c(NA, diff(log(biib_data$biib)))
head(biib_data, 10)

##### 2. 生成TDA_norm数据 #####

library(TDA)

# 1. 准备数据 --------------------------------------------------------
# 假设 biib_data 已经存在，且包含 return_log 列
biib_returns <- na.omit(biib_data$return_log)  # 去掉 NA

# 2. 设置参数 --------------------------------------------------------
window_size <- 50   # 窗口长度 (21 个交易日 ≈ 1 个月)
step_size   <- 1    # 每次滑动 1 天
embed_dim   <- 3    # 延迟嵌入维度
tau         <- 1    # 延迟步长

# 3. 生成滑动窗口 ----------------------------------------------------
slide_mat <- slide_window(biib_returns, sample_size = window_size, step_size = step_size)
cat("生成的滑动窗口矩阵维度: ", dim(slide_mat), "\n")  # 行数=窗口数，列数=窗口长度

# 4. 对第一个窗口做时间延迟嵌入 -------------------------------------
embedded_first <- time_delay_embedding(slide_mat[1,], m = embed_dim, d = tau)
cat("第一个窗口的嵌入矩阵维度: ", dim(embedded_first), "\n")

# 5. 对所有窗口做嵌入并存储为三维数组 -------------------------------
n_windows <- nrow(slide_mat)
Arr_embedded <- array(NA, dim = c(nrow(time_delay_embedding(slide_mat[1,], embed_dim, tau)),
                                  embed_dim,
                                  n_windows))

for (i in 1:n_windows) {
  Arr_embedded[,,i] <- time_delay_embedding(slide_mat[i,], embed_dim, tau)
}

cat("所有窗口点云数组维度: ", dim(Arr_embedded), "\n")

# 6. 计算每个窗口的 TDA norm ----------------------------------------
library(TDA)

# 设置参数
tseq <- seq(0, 0.3, length = 1000)   # persistence landscape 的时间序列
embed_dim <- 3                       # 嵌入维度
tau <- 1                             # 延迟步长
window_size <- 50                    # 滑动窗口长度
step_size <- 1                       # 滑动步长

# 定义函数：计算一个窗口的 TDA norm
compute_tda_norm <- function(window_data, embed_dim = 3, tau = 1, tseq, p = 2) {
  # 延迟嵌入
  embedded <- time_delay_embedding(window_data, m = embed_dim, d = tau)
  
  # 计算持久同调 (Rips 复形)
  vr_ph <- ripsDiag(
    X = embedded,
    maxdimension = 1,
    maxscale = 0.3,
    dist = "euclidean",
    library = "GUDHI",  # 更快更稳
    printProgress = FALSE
  )
  
  # 计算 landscape
  ls <- landscape(vr_ph[["diagram"]], dimension = 1, KK = 1, tseq = tseq)
  
  # TDA norm (L^p 范数)
  norm_val <- (sum(ls^p) * (tseq[2] - tseq[1]))^(1/p)
  return(norm_val)
}

# 对所有窗口进行计算
n_windows <- nrow(slide_mat)
tda_norms <- numeric(n_windows)

for (i in 1:n_windows) {
  tda_norms[i] <- compute_tda_norm(slide_mat[i,], embed_dim, tau, tseq, p = 2)
}

# 构建时间序列结果
tda_norm_series <- data.frame(
  date = biib_data$date[(window_size): (window_size + n_windows - 1)],  # 对齐日期
  tda_norm = tda_norms
)

head(tda_norm_series)


library(ggplot2)

# 绘制 TDA norm 时间序列
p_tda <- ggplot(tda_norm_series, aes(x = date, y = tda_norm)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(color = "darkred", size = 0.6, alpha = 0.5) + # 辅助点
  labs(
    title = "TDA Norm of BIIB Log Returns(window = 50)",
    x = "Date",
    y = "TDA Norm (L2)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_tda)

# # 可选：保存为高清图片（适合论文）
# ggsave("tda_norm_biib50.png", p_tda,
#        width = 9, height = 5, dpi = 300, bg = "white")

