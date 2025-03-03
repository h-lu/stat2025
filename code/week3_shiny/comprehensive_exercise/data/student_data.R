# 生成学生成绩数据

# 创建模拟学生成绩数据
generate_student_data <- function() {
  set.seed(123)
  students <- paste0("学生", 1:20)
  
  # 创建宽格式数据
  data.frame(
    学生 = students,
    学号 = paste0("2023", str_pad(1:20, 4, pad = "0")),
    `2023秋_数学` = round(rnorm(20, mean = 75, sd = 10)),
    `2023秋_语文` = round(rnorm(20, mean = 80, sd = 8)),
    `2023秋_英语` = round(rnorm(20, mean = 78, sd = 12)),
    `2024春_数学` = round(rnorm(20, mean = 78, sd = 10)),
    `2024春_语文` = round(rnorm(20, mean = 82, sd = 8)),
    `2024春_英语` = round(rnorm(20, mean = 80, sd = 12))
  ) %>%
    # 确保成绩在合理范围内
    mutate(across(contains("_"), ~pmin(pmax(.x, 40), 100)))
}

# 使用方法
# 加载tidyverse
# library(tidyverse)
# 生成数据
# student_data <- generate_student_data() 