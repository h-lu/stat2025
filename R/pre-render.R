# 预渲染脚本，在Quarto处理所有文档前执行

# 确保Quarto能够找到并加载所需的包
if (!requireNamespace("ragg", quietly = TRUE)) {
  install.packages("ragg")
}

if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")
}

# 设置knitr使用ragg设备，这个设备对于中文支持更好
options(knitr.graphics.auto_pdf = TRUE)

# 确保图形设备正确加载中文字体
knitr::opts_chunk$set(
  dev = "ragg_png",
  fig.retina = 2,
  dpi = 300
)

# 输出一些信息表明脚本已成功运行
message("Pre-render script completed: 中文字体设置已应用") 