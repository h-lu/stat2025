# 检查并安装必要的包
packages <- c("shiny", "ggplot2", "tidyr", "dplyr", "pwr")

# 安装缺失的包
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 加载所有包
lapply(packages, library, character.only = TRUE) 