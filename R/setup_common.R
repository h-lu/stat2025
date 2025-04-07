# 加载常用R包
library(tidyverse)
library(knitr)
library(kableExtra)
library(scales)
library(patchwork)
library(showtext)
library(ragg)  # 加载更好的图形设备，支持中文

# 设置knitr块选项
knitr::opts_chunk$set(
  dev = "ragg_png",
  fig.retina = 2,
  dpi = 300
)

# Mac字体设置
showtext_auto(enable = TRUE)
font_add("PingFang", "/System/Library/Fonts/PingFang.ttc")
font_add("Helvetica", "/System/Library/Fonts/Helvetica.ttc")

# 确保中文字体在各种设备上都能正确显示
par(family = "PingFang")

# ggplot主题设置
theme_set(
  theme_minimal() +
  theme(
    text = element_text(family = "PingFang"),
    plot.title = element_text(family = "PingFang", face = "bold", size = 14),
    plot.subtitle = element_text(family = "PingFang", size = 12),
    axis.title = element_text(family = "PingFang", size = 11),
    axis.text = element_text(family = "PingFang", size = 10),
    legend.title = element_text(family = "PingFang", size = 11),
    legend.text = element_text(family = "PingFang", size = 10),
    panel.grid.minor = element_blank()
  )
)

# 确保所有后续图表应用中文设置
update_geom_defaults("text", list(family = "PingFang"))
update_geom_defaults("label", list(family = "PingFang"))

# 表格默认设置
options(knitr.kable.NA = "")

# 数据处理设置
options(dplyr.print_min = 6, dplyr.print_max = 10)
options(scipen = 999, digits = 3)

# 随机数种子设置
set.seed(42) 