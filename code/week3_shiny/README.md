# 第三周: 描述性统计 - 数据探索与可视化

这个目录包含了第三周课程内容的交互式课件和练习。

## 目录结构

- `lecture/` - 教师课堂演示用的交互式课件
  - `app.R` - 主应用文件 
  - `data/` - 包含示例数据集
  - `modules/` - 包含各个功能模块（数据清洗、可视化等）

- `basic_exercise/` - 基础练习，帮助学生熟悉基本功能
  - `app.R` - 练习应用程序
  - `exercises/` - 各个练习的内容和解答

- `comprehensive_exercise/` - 综合练习，综合应用所学知识
  - `app.R` - 练习应用程序
  - `data/` - 练习数据集

## 安装与运行

### 环境要求
- R (>= 4.0.0)
- RStudio (推荐)

### 必要的R包
```R
install.packages(c("shiny", "shinydashboard", "tidyverse", "DT", "plotly", "learnr", "palmerpenguins"))
```

### 运行应用
1. 在RStudio中打开对应的`app.R`文件
2. 点击"Run App"按钮，或使用以下命令运行：
```R
shiny::runApp("path/to/app.R")
```

## 使用说明

### 课件演示应用
教师用于课堂演示的交互式应用，涵盖：
- 高级数据清洗与整理 (dplyr, tidyr)
- 基本数据可视化 (ggplot2)
- 数据可视化美化与高级技巧

### 基础练习
包含多个独立练习，对应课程中的各个知识点：
- dplyr高级函数练习
- tidyr数据整理练习
- ggplot2基础图表绘制练习
- 图形美化练习

### 综合练习
基于真实数据集的综合分析案例，要求学生：
- 清洗和整理数据
- 通过可视化探索数据特征
- 根据分析结果回答问题 