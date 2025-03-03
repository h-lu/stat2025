# R语言数据导入与初步理解 - Shiny应用启动脚本
# 本脚本用于启动第二周课程的综合应用

cat("\n===== R语言数据导入与初步理解 - Shiny应用 =====\n\n")
cat("正在准备启动应用...\n\n")

# 检查环境
cat("1. 检查工作目录...\n")
# 直接使用当前工作目录
script_dir <- getwd()
# 如果当前目录不包含code/week2_shiny，则尝试找到正确的路径
if (!dir.exists(file.path(script_dir, "code/week2_shiny")) && !file.exists(file.path(script_dir, "app.R"))) {
  # 如果当前目录是week2_shiny的子目录
  if (basename(script_dir) == "week2_shiny") {
    # 直接使用当前目录
    cat("   已在week2_shiny目录中\n")
  } else if (basename(script_dir) == "modules" && basename(dirname(script_dir)) == "week2_shiny") {
    # 如果在modules目录中，向上一级
    script_dir <- dirname(script_dir)
    cat("   从modules目录向上移动到week2_shiny目录\n")
  } else {
    # 尝试定位week2_shiny目录
    if (dir.exists(file.path(script_dir, "code/week2_shiny"))) {
      script_dir <- file.path(script_dir, "code/week2_shiny")
      cat("   找到week2_shiny目录\n")
    } else {
      cat("   注意: 无法确定正确的week2_shiny目录位置，使用当前工作目录\n")
    }
  }
}

cat("   工作目录:", script_dir, "\n")

# 设置工作目录
setwd(script_dir)
cat("   已将工作目录设置为:", script_dir, "\n\n")

# 安装并加载必要的包
cat("2. 检查必要的R包...\n")
if (file.exists("install_packages.R")) {
  cat("   找到安装包脚本，正在运行...\n")
  source("install_packages.R")
} else {
  cat("   警告: 找不到install_packages.R脚本，将尝试自动安装必要的包\n")
  required_packages <- c("shiny", "shinydashboard", "DT", "readr", "readxl", "dplyr")
  new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(new_packages) > 0) {
    cat("   正在安装缺失的包:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages)
  } else {
    cat("   所有必要的包已安装\n")
  }
}
cat("\n")

# 检查数据文件
cat("3. 检查示例数据文件...\n")
data_files <- c(
  "student_data.csv", "student_data.rds",
  "movie_data.csv", "movie_data.rds",
  "sales_data.csv", "sales_data.rds",
  "sample_datasets.RData"
)
missing_files <- data_files[!file.exists(data_files)]
if (length(missing_files) > 0) {
  cat("   警告: 以下数据文件缺失:", paste(missing_files, collapse = ", "), "\n")
  cat("   如果需要，将尝试生成示例数据\n")
  if (file.exists("sample_data.R")) {
    cat("   找到示例数据生成脚本，正在运行...\n")
    source("sample_data.R")
    cat("   示例数据已重新生成\n")
  } else {
    cat("   错误: 找不到sample_data.R脚本，无法生成示例数据\n")
    cat("   应用可能无法正常运行示例数据功能\n")
  }
} else {
  cat("   所有示例数据文件已存在\n")
}
cat("\n")

# 检查模块文件
cat("4. 检查模块文件...\n")
module_files <- c(
  "modules/import_module.R",
  "modules/view_module.R",
  "modules/analysis_module.R",
  "modules/tutorial_module.R"
)
missing_modules <- module_files[!file.exists(module_files)]
if (length(missing_modules) > 0) {
  cat("   错误: 以下模块文件缺失:", paste(missing_modules, collapse = ", "), "\n")
  cat("   应用可能无法正常运行\n")
} else {
  cat("   所有模块文件已存在\n")
}
cat("\n")

# 启动应用
cat("5. 准备启动应用...\n")
if (!file.exists("app.R")) {
  cat("   错误: 找不到app.R文件，无法启动应用\n")
  stop("找不到app.R文件")
} else {
  cat("   找到app.R文件，正在启动应用...\n\n")
  cat("====================================\n")
  cat("应用已启动！按Ctrl+C可停止应用\n")
  cat("====================================\n\n")
  
  # 启动应用
  shiny::runApp(".", launch.browser = TRUE)
} 