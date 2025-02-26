# 创建示例数据集用于第二周Shiny应用
# 这个脚本创建几个不同类型的数据集，用于演示数据导入、查看和分析

# 设置随机种子以确保可重复性
set.seed(123)

# 1. 创建学生成绩数据集
create_student_data <- function() {
  n <- 100
  students <- data.frame(
    学号 = paste0("STU", sprintf("%03d", 1:n)),
    姓名 = paste0("学生", 1:n),
    性别 = sample(c("男", "女"), n, replace = TRUE, prob = c(0.55, 0.45)),
    年级 = sample(1:4, n, replace = TRUE),
    专业 = sample(c("计算机科学", "数据科学", "统计学", "人工智能", "信息管理"), n, replace = TRUE),
    数学成绩 = round(rnorm(n, mean = 75, sd = 12), 1),
    统计成绩 = round(rnorm(n, mean = 72, sd = 15), 1),
    程序成绩 = round(rnorm(n, mean = 78, sd = 14), 1),
    出勤率 = round(runif(n, min = 0.7, max = 1.0), 2)
  )
  
  # 添加一些缺失值
  missing_indices <- sample(1:n, 10)
  students$数学成绩[missing_indices[1:3]] <- NA
  students$统计成绩[missing_indices[4:6]] <- NA
  students$程序成绩[missing_indices[7:10]] <- NA
  
  # 添加一些异常值
  outlier_indices <- sample(setdiff(1:n, missing_indices), 5)
  students$数学成绩[outlier_indices[1]] <- 100
  students$统计成绩[outlier_indices[2]] <- 32
  students$程序成绩[outlier_indices[3]] <- 28
  students$出勤率[outlier_indices[4:5]] <- c(0.4, 0.5)
  
  return(students)
}

# 2. 创建电影数据集
create_movie_data <- function() {
  n <- 150
  
  # 生成随机的上映日期
  release_dates <- as.Date("2010-01-01") + sample(0:(365*10), n, replace = TRUE)
  
  movies <- data.frame(
    电影ID = paste0("MOV", sprintf("%04d", 1:n)),
    电影名称 = paste0("电影", 1:n),
    导演 = paste0("导演", sample(1:50, n, replace = TRUE)),
    类型 = sample(c("动作", "喜剧", "剧情", "科幻", "恐怖", "爱情", "动画"), n, replace = TRUE),
    上映日期 = release_dates,
    时长 = round(rnorm(n, mean = 120, sd = 25)),
    评分 = round(runif(n, min = 3, max = 9.5), 1),
    票房 = round(rlnorm(n, meanlog = 10, sdlog = 1) / 1e6, 2)  # 单位：百万
  )
  
  # 添加一些缺失值和异常值
  missing_indices <- sample(1:n, 15)
  movies$导演[missing_indices[1:5]] <- NA
  movies$评分[missing_indices[6:10]] <- NA
  movies$票房[missing_indices[11:15]] <- NA
  
  # 异常值
  outlier_indices <- sample(setdiff(1:n, missing_indices), 5)
  movies$时长[outlier_indices[1:2]] <- c(240, 250)
  movies$评分[outlier_indices[3:4]] <- c(10, 1.5)
  movies$票房[outlier_indices[5]] <- 1200  # 一个超级大片
  
  return(movies)
}

# 3. 创建销售数据集
create_sales_data <- function() {
  # 创建一年的销售数据，按天记录
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)
  
  # 创建一个周期性趋势
  base_trend <- sin(2 * pi * (1:n) / 365) * 100 + 500
  
  # 添加一些随机波动
  sales <- base_trend + rnorm(n, mean = 0, sd = 50)
  sales <- round(pmax(sales, 200), 0)  # 确保最小值不小于200
  
  # 添加一些特殊日期的销售高峰
  special_dates <- as.Date(c("2023-01-01", "2023-02-14", "2023-05-01", "2023-06-18", 
                             "2023-10-01", "2023-11-11", "2023-12-12", "2023-12-25"))
  for (date in special_dates) {
    idx <- which(dates == date)
    if (length(idx) > 0) {
      sales[idx] <- sales[idx] * runif(1, min = 1.5, max = 3.0)
    }
  }
  
  sales_data <- data.frame(
    日期 = dates,
    销售额 = sales,
    订单数 = round(sales / runif(n, min = 15, max = 25)),
    客户数 = round(sales / runif(n, min = 25, max = 40)),
    产品类别 = sample(c("电子产品", "服装", "家居", "食品", "图书"), n, replace = TRUE),
    促销活动 = ifelse(dates %in% special_dates, "是", "否")
  )
  
  # 转换促销活动为逻辑型
  sales_data$促销活动 <- sales_data$促销活动 == "是"
  
  return(sales_data)
}

# 生成数据并保存
student_data <- create_student_data()
movie_data <- create_movie_data()
sales_data <- create_sales_data()

# 将数据保存为不同格式
write.csv(student_data, "code/week2_shiny/student_data.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(movie_data, "code/week2_shiny/movie_data.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sales_data, "code/week2_shiny/sales_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 也保存为RDS格式
saveRDS(student_data, "code/week2_shiny/student_data.rds")
saveRDS(movie_data, "code/week2_shiny/movie_data.rds")
saveRDS(sales_data, "code/week2_shiny/sales_data.rds")

# 以R对象形式提供数据
sample_datasets <- list(
  student_data = student_data,
  movie_data = movie_data,
  sales_data = sales_data
)

save(sample_datasets, file = "code/week2_shiny/sample_datasets.RData") 