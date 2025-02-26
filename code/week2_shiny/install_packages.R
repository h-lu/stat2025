# 安装Shiny应用依赖包

# 检查并安装必要的包
required_packages <- c(
  # Shiny相关包
  "shiny", 
  "shinydashboard", 
  "DT",
  
  # 数据导入相关包
  "readr", 
  "readxl", 
  "jsonlite",
  
  # 数据处理相关包
  "dplyr", 
  "rlang",
  "data.table"
)

# 安装缺失的包
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages) > 0) {
  message("正在安装以下包: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
} else {
  message("所有必要的包已安装")
}

# 检查是否所有包都可以成功加载
load_check <- sapply(required_packages, function(pkg) {
  tryCatch({
    library(pkg, character.only = TRUE)
    TRUE
  }, error = function(e) {
    message("无法加载包: ", pkg, " - ", e$message)
    FALSE
  })
})

if(all(load_check)) {
  message("所有包已成功加载，应用准备就绪!")
} else {
  message("某些包无法加载，可能会影响应用的运行。请检查上面的错误信息。")
} 