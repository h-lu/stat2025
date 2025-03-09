# 推断性统计综合应用程序
# 整合置信区间、假设检验、功效分析和t检验的演示

# 加载必要的包
library(shiny)
library(shinydashboard)
library(tidyverse)
library(pwr)
library(showtext)
library(DT)

# 加载模块
source("modules/confidence_interval.R")
source("modules/hypothesis_testing.R")
source("modules/power_analysis.R")
source("modules/t_test.R")
source("modules/basic_exercises.R")
source("modules/advanced_exercises.R")

# 加载辅助函数
source("helpers/plotting_helpers.R")
source("helpers/statistics_helpers.R")
source("helpers/ui_helpers.R")
source("global.R")

# 自定义CSS样式
custom_css <- "
  /* 全局样式 */
  body {
    font-family: 'Roboto', sans-serif;
    background-color: #f5f7fa;
    color: #333;
  }
  
  /* 顶部导航栏 */
  .skin-blue .main-header .logo {
    background-color: #2c3e50;
    font-weight: 600;
    font-family: 'Noto Sans SC', sans-serif;
  }
  
  .skin-blue .main-header .logo:hover {
    background-color: #2c3e50;
  }
  
  .skin-blue .main-header .navbar {
    background-color: #34495e;
  }
  
  /* 侧边栏 */
  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #2c3e50;
  }
  
  .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
    background-color: #1a2530;
    border-left-color: #3498db;
  }
  
  .skin-blue .sidebar a {
    color: #ecf0f1;
  }
  
  /* box样式 */
  .box {
    border-radius: 8px;
    box-shadow: 0 4px 8px rgba(0,0,0,0.05);
    border-top: 3px solid #3498db;
  }
  
  .box.box-primary {
    border-top-color: #3498db;
  }
  
  .box.box-success {
    border-top-color: #2ecc71;
  }
  
  .box.box-warning {
    border-top-color: #f1c40f;
  }
  
  .box.box-danger {
    border-top-color: #e74c3c;
  }
  
  .box.box-info {
    border-top-color: #1abc9c;
  }
  
  /* 按钮样式 */
  .btn-primary {
    background-color: #3498db;
    border-color: #2980b9;
  }
  
  .btn-primary:hover {
    background-color: #2980b9;
    border-color: #2980b9;
  }
  
  .btn-success {
    background-color: #2ecc71;
    border-color: #27ae60;
  }
  
  .btn-success:hover {
    background-color: #27ae60;
    border-color: #27ae60;
  }
  
  /* 输入控件 */
  .form-control {
    border-radius: 4px;
    border: 1px solid #dce4ec;
  }
  
  .form-control:focus {
    border-color: #3498db;
    box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
  }
  
  /* 选项卡样式 */
  .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
    border-top: 3px solid #3498db;
    border-radius: 4px 4px 0 0;
  }
  
  /* 数据表格样式 */
  .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, 
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, 
  .dataTables_wrapper .dataTables_paginate {
    color: #7b8a8b;
  }
  
  .dataTables_wrapper .dataTables_paginate .paginate_button.current, 
  .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
    background: #3498db;
    color: white !important;
    border: 1px solid #3498db;
    border-radius: 4px;
  }
"

# 应用UI
ui <- dashboardPage(
  dashboardHeader(title = "推断性统计综合演示"),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    sidebarMenu(
      menuItem("首页", tabName = "home", icon = icon("home")),
      menuItem("置信区间", tabName = "conf_int", icon = icon("chart-line")),
      menuItem("假设检验错误", tabName = "hyp_test_errors", icon = icon("exclamation-triangle")),
      menuItem("检验功效分析", tabName = "power_analysis", icon = icon("tachometer-alt")),
      menuItem("t检验", tabName = "t_test", icon = icon("balance-scale")),
      menuItem("基础练习", tabName = "basic_exercises", icon = icon("tasks")),
      menuItem("综合练习", tabName = "advanced_exercises", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap")
    ),
    tabItems(
      # 首页
      tabItem(tabName = "home",
              fluidRow(
                create_title_card(
                  title = "推断性统计综合演示应用",
                  subtitle = "探索置信区间、假设检验错误、检验功效和t检验",
                  icon = "chart-bar"
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  
                  div(
                    style = "padding: 10px 15px;",
                    h3("欢迎使用推断性统计综合演示应用", style = "font-weight: 600; font-family: 'notosans';"),
                    p("本应用包含以下模块：", style = "font-size: 16px; font-family: 'notosans';"),
                    tags$ul(
                      style = "font-size: 16px; font-family: 'notosans'; line-height: 1.8;",
                      tags$li(strong("置信区间："), "演示置信区间的概念和影响因素"),
                      tags$li(strong("假设检验错误："), "演示Type I和Type II错误的概念"),
                      tags$li(strong("检验功效分析："), "演示如何进行检验功效分析和确定样本量"),
                      tags$li(strong("t检验："), "演示三种t检验的应用场景和结果解释"),
                      tags$li(strong("基础练习："), "提供基础练习题，巩固推断性统计的基本概念"),
                      tags$li(strong("综合练习："), "提供综合练习题，应用推断性统计的方法解决实际问题")
                    ),
                    p("请从左侧菜单选择要浏览的模块。", style = "font-size: 16px; font-family: 'notosans';")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  create_stat_card(
                    title = "置信区间",
                    value = "95%",
                    subtitle = "最常用的置信水平",
                    color = app_colors$chart_primary
                  )
                ),
                column(
                  width = 4,
                  create_stat_card(
                    title = "显著性水平",
                    value = "0.05",
                    subtitle = "常用的α值",
                    color = app_colors$chart_danger
                  )
                ),
                column(
                  width = 4,
                  create_stat_card(
                    title = "检验功效",
                    value = "0.8",
                    subtitle = "推荐的最小功效值",
                    color = app_colors$chart_success
                  )
                )
              )
      ),
      
      # 置信区间模块
      tabItem(tabName = "conf_int",
              confidenceIntervalUI("conf_int_mod")
      ),
      
      # 假设检验错误模块
      tabItem(tabName = "hyp_test_errors",
              hypothesisTestingErrorsUI("hyp_errors_mod")
      ),
      
      # 检验功效分析模块
      tabItem(tabName = "power_analysis",
              powerAnalysisUI("power_analysis_mod")
      ),
      
      # t检验模块
      tabItem(tabName = "t_test",
              tTestUI("t_test_mod")
      ),
      
      # 基础练习模块
      tabItem(tabName = "basic_exercises",
              basicExercisesUI("basic_exercises_mod")
      ),
      
      # 综合练习模块
      tabItem(tabName = "advanced_exercises",
              advancedExercisesUI("advanced_exercises_mod")
      )
    )
  ),
  skin = "blue"
)

# 应用服务器
server <- function(input, output, session) {
  # 调用各模块的服务器函数
  confidenceIntervalServer("conf_int_mod")
  hypothesisTestingErrorsServer("hyp_errors_mod")
  powerAnalysisServer("power_analysis_mod")
  tTestServer("t_test_mod")
  basicExercisesServer("basic_exercises_mod")
  advancedExercisesServer("advanced_exercises_mod")
}

# 运行应用
shinyApp(ui, server) 