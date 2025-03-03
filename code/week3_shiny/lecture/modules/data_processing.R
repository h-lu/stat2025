# 数据处理模块
# 包含数据加载和预处理功能

library(tidyverse)
library(palmerpenguins)

# 自定义函数: 加载并预处理Palmer Penguins数据集
load_penguins_data <- function() {
  # 基础数据集
  data <- palmerpenguins::penguins
  
  # 添加中文标签映射
  data <- data %>%
    mutate(species_zh = case_when(
      species == "Adelie" ~ "阿德利企鹅",
      species == "Chinstrap" ~ "帽带企鹅",
      species == "Gentoo" ~ "巴布亚企鹅",
      TRUE ~ NA_character_
    )) %>%
    mutate(island_zh = case_when(
      island == "Torgersen" ~ "托格森岛",
      island == "Biscoe" ~ "比斯科岛",
      island == "Dream" ~ "梦想岛",
      TRUE ~ NA_character_
    )) %>%
    mutate(sex_zh = case_when(
      sex == "male" ~ "雄性",
      sex == "female" ~ "雌性",
      TRUE ~ NA_character_
    ))
  
  return(data)
}

# 自定义函数: 创建转换后的数据示例（宽格式和长格式转换）
create_pivot_example <- function() {
  # 创建宽格式示例数据
  wide_data <- data.frame(
    学生 = c("小明", "小红", "小李"),
    语文 = c(85, 92, 78),
    数学 = c(90, 88, 95),
    英语 = c(82, 90, 85)
  )
  
  # 创建长格式示例数据
  long_data <- wide_data %>%
    pivot_longer(
      cols = c(语文, 数学, 英语),
      names_to = "科目",
      values_to = "成绩"
    )
  
  return(list(wide_data = wide_data, long_data = long_data))
}

# 自定义函数: 创建分组汇总示例
create_grouped_summary <- function(data) {
  if (missing(data)) {
    data <- load_penguins_data()
  }
  
  # 按种类和性别分组汇总
  summary_data <- data %>%
    group_by(species, sex) %>%
    summarise(
      平均体重 = mean(body_mass_g, na.rm = TRUE),
      平均嘴峰长度 = mean(bill_length_mm, na.rm = TRUE),
      平均嘴峰深度 = mean(bill_depth_mm, na.rm = TRUE),
      样本数 = n(),
      .groups = "drop"
    ) %>%
    arrange(species, sex)
  
  return(summary_data)
} 