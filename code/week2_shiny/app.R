# 第二周：R语言数据导入与数据初步理解 - Shiny综合应用
# 本应用整合了第二周课程内容，包括数据导入、初步查看数据、描述性统计和dplyr基本操作

# 加载必要的包
library(shiny)
library(shinydashboard)
library(DT)

# 加载模块文件
source("modules/import_module.R")
source("modules/view_module.R")
source("modules/analysis_module.R")
source("modules/tutorial_module.R")

# 定义UI
ui <- dashboardPage(
  dashboardHeader(title = "R数据导入与初步理解"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("介绍", tabName = "intro", icon = icon("info-circle")),
      menuItem("数据导入", tabName = "import", icon = icon("file-import")),
      menuItem("数据查看", tabName = "view", icon = icon("table")),
      menuItem("数据分析", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("学习资料", tabName = "tutorial", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # 介绍页面
      tabItem(tabName = "intro",
        fluidRow(
          box(
            title = "欢迎使用R数据导入与初步理解应用", width = 12, status = "primary",
            h3("第二周课程综合应用"),
            p("本应用是为了帮助你学习和实践第二周课程内容而设计的交互式工具。"),
            p("通过本应用，你可以："),
            tags$ul(
              tags$li("导入各种格式的数据（CSV, Excel, RDS等）"),
              tags$li("查看和理解数据结构"),
              tags$li("检查数据质量问题（缺失值、异常值等）"),
              tags$li("计算描述性统计量"),
              tags$li("使用dplyr进行基础数据操作")
            ),
            p("每个功能模块都附带相应的R代码，便于你学习和复用。"),
            
            h4("如何使用本应用"),
            p("1. 在左侧菜单中选择你想使用的功能"),
            p('2. 先在"数据导入"页面导入数据'),
            p("3. 然后在其他页面对导入的数据进行操作"),
            p("4. 查看操作结果和相应的R代码"),
            p('5. 可以访问"学习资料"页面获取更多学习内容和练习'),
            
            h4("示例数据集"),
            p("本应用提供了三个示例数据集："),
            tags$ul(
              tags$li("学生成绩数据：包含学生的基本信息和各科成绩"),
              tags$li("电影数据：包含电影的基本信息、评分和票房"),
              tags$li("销售数据：包含一年的销售记录数据")
            ),
            p("这些数据集包含了一些常见的数据质量问题（缺失值、异常值等），可以用来练习数据处理技能。")
          )
        )
      ),
      
      # 数据导入页面
      tabItem(tabName = "import",
        importUI("import_data")
      ),
      
      # 数据查看页面
      tabItem(tabName = "view",
        viewUI("view_data")
      ),
      
      # 数据分析页面
      tabItem(tabName = "analysis",
        analysisUI("analysis_data")
      ),
      
      # 学习资料页面
      tabItem(tabName = "tutorial",
        tutorialUI("tutorial")
      )
    )
  )
)

# 定义服务器
server <- function(input, output, session) {
  # 导入数据模块
  imported_data <- importServer("import_data")
  
  # 数据查看模块
  viewServer("view_data", imported_data)
  
  # 数据分析模块
  analysisServer("analysis_data", imported_data)
  
  # 学习资料模块
  tutorialServer("tutorial")
}

# 运行应用
shinyApp(ui, server) 