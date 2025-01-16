##### packages #####
install.packages("TDA")
install.packages("scatterplot3d")
install.packages("philentropy")
library(philentropy)
library(TDA)
library(ggplot2)
library(scatterplot3d)
library(TDA)
# 所有可能用到的函数库
slide_window <- function(ts_data, sample_size, step_size) {
  num_samples <- (length(ts_data) - sample_size) %/% step_size +1  # 计算样本总数
  samples <- matrix(NA, nrow = num_samples, ncol = sample_size)
  for (i in 1:num_samples) {
    start_index <- (i - 1) * step_size + 1 # 起始位置的索引
    end_index <- start_index + sample_size - 1 # 结束位置的索引
    if (end_index <= length(ts_data)) {
      samples[i, ] <- ts_data[start_index:end_index]
    }
  }
  return(samples)
}
time_delay_embedding <- function(x, m, d) {
  # x: 输入的时间序列数据
  # m: 嵌入维度
  # d: 延迟步长
  
  # 计算嵌入矩阵的行数
  n <- length(x) - (m - 1) * d
  if(n <= 0)
    stop("Insufficient observations for the requested embedding")
  
  # 初始化嵌入矩阵
  embedded_matrix <- matrix(NA, nrow = n, ncol = m)
  
  # 填充嵌入矩阵
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {embedded_matrix[i, j] <- x[i + (j - 1) * d]}
  }
  
  return(embedded_matrix)
}
# 对形成的点云数据计算persistence homology-landscape-TDA norm
tda_norm_function <- function(landscape,p){
  (sum(landscape^p) * (tseq[2] - tseq[1]))^(1/p)
}

##### 计算
##### 用VR complex计算欧氏距离得到0维同调，计算0维、1阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:n){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 1, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,1)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  tda_norm <- data.frame(t(close_tda_norm))
  colnames(tda_norm)<- c( "close_tda_norm")
  file_name <- basename(file)
  name_without_extension <- tools::file_path_sans_ext(file_name)
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0011", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到0维同调，计算0维、2阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 1, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0012", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}
##### 用VR complex计算欧氏距离得到0维同调，计算0维、2阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 2, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,1)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0021", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到0维同调，计算0维、2阶、L2的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 2, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0022", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到0维同调，计算0维、3阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 3, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,1)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0031", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}


##### 用VR complex计算欧氏距离得到0维同调，计算0维、3阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 3, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0032", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到0维同调，计算0维、1阶、L2的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:n){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 1, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  tda_norm<- data.frame(t(close_tda_norm))
  colnames(tda_norm)<- c( "close_tda_norm")
  file_name <- basename(file)
  name_without_extension <- tools::file_path_sans_ext(file_name)
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0012", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到1维同调，计算1维、1阶、L2的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:n){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 0, KK = 1, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  tda_norm<- data.frame(t(close_tda_norm))
  colnames(tda_norm)<- c( "close_tda_norm")
  file_name <- basename(file)
  name_without_extension <- tools::file_path_sans_ext(file_name)
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_0012", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到1维同调，计算1维、1阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 1, KK = 1, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,1)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_1111", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}


##### 用VR complex计算欧氏距离得到1维同调，计算1维、2阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 1, KK = 2, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,1)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_1121", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到1维同调，计算1维、2阶、L2的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 1, KK = 2, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_1122", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到1维同调，计算1维、3阶、L1的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 1, KK = 3, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,1)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_1131", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

##### 用VR complex计算欧氏距离得到1维同调，计算1维、3阶、L2的持续景观 #####
# modified_data所在目录
folder_path <- "/Users/yaojian/tda_ph_timeserise/data/Modified_data"
# 获取文件夹中所有CSV文件的文件名
modified_data_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 创建一个空列表来存储读取的数据框
data_list <- list()
# 预设一个list来储存tda_norm数据
close_tda_norm <- list()
# 计算全部的norm
for (file in modified_data_list) {
  data <- read.csv(file)
  data_list[[basename(file)]] <- data  # 得到data_list存储数据框的列表
  # 对Close数据进行slide_window操作
  slide_diff_data <- slide_window(data$Close, 21, 1) # 每隔1天取样取样21个数据点
  # 建立数组，每一个矩阵形成一个点云
  n <- nrow(slide_diff_data) # 确定数组的三维
  Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
  # 对获取到的样本数据执行time_dalay_embedding算法，得到点云数据集
  for(i in 1:nrow(slide_diff_data)){
    embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
    Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
  }
  # 遍历每一行生成计算tda_norm
  for(i in 1:dim(Arr_embedded_slide_diff_data)[3]){
    vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,i], maxdimension = 1, maxscale = 80, dist = "euclidean")
    landscape_values <- landscape(vr_ph_data$diagram, dimension = 1, KK = 3, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
    tda_norm <- tda_norm_function(landscape_values,2)
    close_tda_norm[i] <- tda_norm
  }
  close_tda_norm <- data.frame(close_tda_norm)
  # 转置
  tda_norm<- data.frame(t(close_tda_norm))
  # 改变列名
  colnames(tda_norm)<- c( "close_tda_norm")
  # 保存
  # 将修改后的close_tda_norm数据框保存为CSV文件
  # 导出到close_price_tda_norm所在目录
  file_name <- basename(file)
  # 去掉文件后缀
  name_without_extension <- tools::file_path_sans_ext(file_name)
  # 保存到计算的tda_norm到文件夹
  output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/close_price_tda_norm_test/VR_1132", paste0("close_tda_norm_",name_without_extension))
  write.csv(tda_norm, file = output_path, row.names = FALSE)
}

###### 2 以TSLA为例绘制3D点云数据图#####
TSLA_data <- unlist(log_return[44])
slide_diff_data <- slide_window(TSLA_data, 21, 1)
n <- nrow(slide_diff_data) # 确定数组的三维
Arr_embedded_slide_diff_data <- array(0, dim = c(19,3,n))
print(slide_diff_data[1,])
for(i in 1: n){
  embedded_slide_diff_data <- time_delay_embedding(slide_diff_data[i,], 3, 1)
  Arr_embedded_slide_diff_data[,,i] <- embedded_slide_diff_data
}
print(Arr_embedded_slide_diff_data)
# 2.1 将三维点云绘制在欧式空间
# 初始化x,y,z向量
x <- c()
y <- c()
z <- c()
for(i in 1:19){
  x <- c(x,Arr_embedded_slide_diff_data[i,1,1])
  y <- c(y,Arr_embedded_slide_diff_data[i,2,1])
  z <- c(z,Arr_embedded_slide_diff_data[i,3,1])
}
output_path <- file.path("/Users/yaojian/tda_ph_timeserise/data/Plot_paper/new_TSLA_T1_point_cloud.png")
png(filename = output_path, width = 1200, height = 900, units = "px")
scatterplot3d(x, y, z,
              pch = 20, 
              color = "#d23918", 
              xlab = "x",
              ylab = "y",
              zlab = "z",
              cex.main = 1.5,
              cex.lab = 1.2,
              cex.axis = 0.5,
              lwd = 8,
              grid = TRUE,
              box = TRUE,
              main = "TSLA T1_Point cloud", 
              type ="b")
# 关闭设备以完成文件保存
dev.off()

# 2.2 用ripsDiag进行过滤
vr_ph_data <- ripsDiag(Arr_embedded_slide_diff_data[,,1], maxdimension = 1, maxscale = 0.3, dist = "euclidean")
print(vr_ph_data)
vr_ph_result <- data.frame(vr_ph_data)
# 2.3 绘制持续图
# 设置保存路径
output_path <- "/Users/yaojian/tda_ph_timeserise/data/Plot_paper/new_TSLA_T1_persistence_diagram.png"
png(filename = output_path, width= 800, height = 600, units = "px")
plot(vr_ph_data$diagram, main = "Persistence Diagram")
dev.off()

library(TDA)

# 绘制 Persistence Diagram
plot(vr_ph_data$diagram, main = "Persistence Diagram")

library(ggplot2)

# 转换为 data.frame
df <- data.frame(Dimension = factor(vr_ph_data$diagram[, 1]), 
                 Birth = vr_ph_data$diagram[, 2], 
                 Death = vr_ph_data$diagram[, 3])

# 绘图
ggplot(df, aes(x = Birth, y = Death, color = Dimension)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  ggtitle("Persistence Diagram") +
  xlab("Birth") +
  ylab("Death")

# 2.4 绘制条形码图
# 手动提取条形码数据并绘图
# 提取 diagram 数据
diagram_data <- vr_ph_data$diagram
print(diagram_data)
output_path <- "/Users/yaojian/tda_ph_timeserise/data/Plot_paper/new_TSLA_T1_barcode_code.png"
png(filename = output_path, width= 1600, height = 900, units = "px")
plot(vr_ph_data, barcode = TRUE, main = "Barcode code")
dev.off 

# 设置保存路径
output_path <- "/Users/yaojian/tda_ph_timeserise/data/Plot_paper/TSLA_T1_persistence_code.png"
# 打开 PNG 图形设备
png(filename = output_path, width = 1600, height = 900, units = "px" )
# 创建颜色向量，依据第一列进行分类
colors <- ifelse(diagram_data[, 1] == 0, "#b36a6f", "#a1d0c7")
# 手动绘制条形码获得持续时间的柱状图
barplot(
  height = diagram_data[, 3] - diagram_data[, 2],
  names.arg = seq_len(nrow(diagram_data)),
  main = "Barcode Diagram",
  xlab = "Bars",
  ylab = "Persistence",
  col = colors,
  border = NA,
  # type = "p",
)
# 关闭图形设备
dev.off()
print(vr_ph_data$diagram)
# 2.5 绘制landscape的图
landscape_values1 <- landscape(vr_ph_data$diagram, dimension = 1, KK = 1, tseq <- seq(0, 10, length = 1000)) # KK决定了我们关注的是第几层拓扑特征，tseq决定了我们在哪些时间点上去评估这些特征。
landscape_values2 <- landscape(vr_ph_data$diagram, dimension = 1, KK = 2, tseq <- seq(0, 10, length = 1000))

plot(tseq, landscape_values1, type = "l", col = "#c52a20", xlab ="t", ylab = "landscape")

tda_norm <- tda_norm_function(landscape_values1,2)

##### 3. 绘制论文图2 #####










