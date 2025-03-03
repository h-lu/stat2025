# 第三周：描述性统计 - 数据探索与可视化
# 基础练习应用

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(learnr)
library(palmerpenguins)

# 定义UI
ui <- dashboardPage(
  # 仪表板标题
  dashboardHeader(title = "第三周：基础练习"),
  
  # 侧边栏
  dashboardSidebar(
    sidebarMenu(
      menuItem("练习说明", tabName = "intro", icon = icon("info-circle")),
      menuItem("知识图谱", tabName = "knowledge_map", icon = icon("project-diagram")),
      menuItem("dplyr练习", tabName = "dplyr", icon = icon("filter")),
      menuItem("tidyr练习", tabName = "tidyr", icon = icon("table")),
      menuItem("ggplot2练习", tabName = "ggplot2", icon = icon("chart-bar"))
    )
  ),
  
  # 主体内容
  dashboardBody(
    tabItems(
      # 练习说明页面
      tabItem(tabName = "intro",
        fluidRow(
          box(
            title = "基础练习说明", width = 12, status = "primary",
            p("本应用包含了第三周课程内容的基础练习，帮助你巩固所学知识。练习分为三个部分："),
            tags$ol(
              tags$li(strong("dplyr练习："), "测试你对dplyr包高级函数的理解和应用能力。"),
              tags$li(strong("tidyr练习："), "测试你对tidyr包数据整理函数的理解和应用能力。"),
              tags$li(strong("ggplot2练习："), "测试你对ggplot2包数据可视化的理解和应用能力。")
            ),
            p("每个部分都包含选择题和编程练习，你可以通过左侧菜单进行导航。"),
            p("完成所有练习后，你将能够："),
            tags$ul(
              tags$li("熟练使用dplyr包的高级函数进行数据清洗和转换"),
              tags$li("熟练使用tidyr包的函数进行数据整理和重塑"),
              tags$li("熟练使用ggplot2包创建各种类型的数据可视化图表")
            )
          )
        ),
        fluidRow(
          box(
            title = "数据集介绍", width = 12, status = "info",
            p("本练习主要使用Palmer Penguins数据集。该数据集包含了南极洲Palmer站点附近三种企鹅的测量数据。"),
            p("数据集包含以下变量："),
            tags$ul(
              tags$li("species: 企鹅种类 (Adelie, Chinstrap, Gentoo)"),
              tags$li("island: 岛屿 (Torgersen, Biscoe, Dream)"),
              tags$li("bill_length_mm: 嘴峰长度 (mm)"),
              tags$li("bill_depth_mm: 嘴峰深度 (mm)"),
              tags$li("flipper_length_mm: 鳍长度 (mm)"),
              tags$li("body_mass_g: 体重 (g)"),
              tags$li("sex: 性别 (male, female)"),
              tags$li("year: 观测年份")
            ),
            p("数据预览："),
            DTOutput("data_preview")
          )
        ),
        fluidRow(
          box(
            title = "数据分析流程", width = 12, status = "success",
            p("在开始练习前，请先了解数据分析的基本流程："),
            tags$img(src = "data_analysis_flow.svg", width = "100%", height = "auto")
          )
        )
      ),
      
      # 知识图谱页面
      tabItem(tabName = "knowledge_map",
        fluidRow(
          box(
            title = "数据分析流程", width = 12, status = "warning",
            p("数据分析是一个循环迭代的过程，包含以下几个关键步骤："),
            tags$img(src = "data_analysis_flow.svg", width = "100%", height = "auto")
          )
        ),
        fluidRow(
          tabBox(
            title = "核心包知识图谱", width = 12,
            tabPanel("dplyr包", 
                    p("dplyr包是数据转换的核心工具，提供了一系列用于数据操作的函数。"),
                    tags$img(src = "dplyr_map.svg", width = "100%", height = "auto")),
            tabPanel("tidyr包", 
                    p("tidyr包用于数据整理，帮助创建整洁数据。"),
                    tags$img(src = "tidyr_map.svg", width = "100%", height = "auto")),
            tabPanel("ggplot2包", 
                    p("ggplot2包基于图形语法，用于创建各种类型的数据可视化。"),
                    tags$img(src = "ggplot2_map.svg", width = "100%", height = "auto"))
          )
        )
      ),
      
      # dplyr练习页面
      tabItem(tabName = "dplyr",
        fluidRow(
          box(
            title = "dplyr高级函数练习", width = 12, status = "primary",
            p("本练习将测试你对dplyr包高级函数的理解和应用能力，包括mutate(), arrange(), group_by(), summarise()等函数。"),
            p("完成以下练习，巩固你对dplyr包的掌握。")
          )
        ),
        fluidRow(
          box(
            title = "练习内容", width = 12, status = "info",
            uiOutput("dplyr_exercises")
          )
        )
      ),
      
      # tidyr练习页面
      tabItem(tabName = "tidyr",
        fluidRow(
          box(
            title = "tidyr数据整理练习", width = 12, status = "primary",
            p("本练习将测试你对tidyr包数据整理函数的理解和应用能力，包括pivot_longer(), pivot_wider(), separate(), unite()等函数。"),
            p("完成以下练习，巩固你对tidyr包的掌握。")
          )
        ),
        fluidRow(
          box(
            title = "练习内容", width = 12, status = "info",
            uiOutput("tidyr_exercises")
          )
        )
      ),
      
      # ggplot2练习页面
      tabItem(tabName = "ggplot2",
        fluidRow(
          box(
            title = "ggplot2数据可视化练习", width = 12, status = "primary",
            p("本练习将测试你对ggplot2包数据可视化的理解和应用能力，包括基本图表类型、图形美化、分面和图层等。"),
            p("完成以下练习，巩固你对ggplot2包的掌握。")
          )
        ),
        fluidRow(
          box(
            title = "练习内容", width = 12, status = "info",
            uiOutput("ggplot2_exercises")
          )
        )
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  # 数据预览
  output$data_preview <- renderDT({
    datatable(head(penguins, 10), options = list(scrollX = TRUE))
  })
  
  # 练习内容
  output$dplyr_exercises <- renderUI({
    tagList(
      h3("选择题"),
      radioButtons("dplyr_q1", "1. 以下哪个函数用于选择数据框中的列？",
                  choices = c("select()", "filter()", "arrange()", "mutate()")),
      radioButtons("dplyr_q2", "2. 以下哪个函数用于根据条件筛选行？",
                  choices = c("select()", "filter()", "arrange()", "mutate()")),
      radioButtons("dplyr_q3", "3. 以下哪个函数用于创建新变量？",
                  choices = c("select()", "filter()", "arrange()", "mutate()")),
      actionButton("submit_dplyr_quiz", "提交选择题", class = "btn-primary"),
      uiOutput("dplyr_quiz_feedback"),
      h3("编程练习"),
      p("提示：使用group_by()和summarise()函数来计算每种企鹅的平均嘴峰长度和深度。"),
      textAreaInput("dplyr_code", "编写代码：计算每种企鹅的平均嘴峰长度和深度",
                   height = "200px"),
      actionButton("run_dplyr", "运行代码", class = "btn-primary"),
      verbatimTextOutput("dplyr_result"),
      actionButton("submit_dplyr_code", "提交代码", class = "btn-success"),
      uiOutput("dplyr_code_feedback")
    )
  })
  
  output$tidyr_exercises <- renderUI({
    tagList(
      h3("选择题"),
      radioButtons("tidyr_q1", "1. 以下哪个函数用于将宽格式数据转换为长格式？",
                  choices = c("pivot_longer()", "pivot_wider()", "separate()", "unite()")),
      radioButtons("tidyr_q2", "2. 以下哪个函数用于将长格式数据转换为宽格式？",
                  choices = c("pivot_longer()", "pivot_wider()", "separate()", "unite()")),
      radioButtons("tidyr_q3", "3. 以下哪个函数用于拆分列？",
                  choices = c("pivot_longer()", "pivot_wider()", "separate()", "unite()")),
      actionButton("submit_tidyr_quiz", "提交选择题", class = "btn-primary"),
      uiOutput("tidyr_quiz_feedback"),
      h3("编程练习"),
      p("提示：使用pivot_longer()函数将bill_length_mm和bill_depth_mm转换为长格式。"),
      textAreaInput("tidyr_code", "编写代码：将企鹅数据中的bill_length_mm和bill_depth_mm转换为长格式",
                   height = "200px"),
      actionButton("run_tidyr", "运行代码", class = "btn-primary"),
      verbatimTextOutput("tidyr_result"),
      actionButton("submit_tidyr_code", "提交代码", class = "btn-success"),
      uiOutput("tidyr_code_feedback")
    )
  })
  
  output$ggplot2_exercises <- renderUI({
    tagList(
      h3("选择题"),
      radioButtons("ggplot2_q1", "1. 以下哪个函数用于创建散点图？",
                  choices = c("geom_point()", "geom_line()", "geom_bar()", "geom_boxplot()")),
      radioButtons("ggplot2_q2", "2. 以下哪个函数用于创建线图？",
                  choices = c("geom_point()", "geom_line()", "geom_bar()", "geom_boxplot()")),
      radioButtons("ggplot2_q3", "3. 以下哪个函数用于创建条形图？",
                  choices = c("geom_point()", "geom_line()", "geom_bar()", "geom_boxplot()")),
      actionButton("submit_ggplot2_quiz", "提交选择题", class = "btn-primary"),
      uiOutput("ggplot2_quiz_feedback"),
      h3("编程练习"),
      p("提示：使用ggplot()创建图形，aes()设置映射，geom_point()创建散点图，color参数设置颜色。"),
      textAreaInput("ggplot2_code", "编写代码：创建企鹅嘴峰长度与深度的散点图，按种类着色",
                   height = "200px"),
      actionButton("run_ggplot2", "运行代码", class = "btn-primary"),
      plotOutput("ggplot2_result"),
      actionButton("submit_ggplot2_code", "提交代码", class = "btn-success"),
      uiOutput("ggplot2_code_feedback")
    )
  })
  
  # 运行代码
  observeEvent(input$run_dplyr, {
    output$dplyr_result <- renderText({
      tryCatch({
        eval(parse(text = input$dplyr_code))
        "代码运行成功！"
      }, error = function(e) {
        paste("错误：", e$message)
      })
    })
  })
  
  observeEvent(input$run_tidyr, {
    output$tidyr_result <- renderText({
      tryCatch({
        eval(parse(text = input$tidyr_code))
        "代码运行成功！"
      }, error = function(e) {
        paste("错误：", e$message)
      })
    })
  })
  
  observeEvent(input$run_ggplot2, {
    output$ggplot2_result <- renderPlot({
      tryCatch({
        eval(parse(text = input$ggplot2_code))
      }, error = function(e) {
        plot(0, 0, type = "n", xlab = "", ylab = "")
        text(0, 0, paste("错误：", e$message), col = "red")
      })
    })
  })
  
  # 提交选择题反馈
  observeEvent(input$submit_dplyr_quiz, {
    correct_answers <- c("select()", "filter()", "mutate()")
    user_answers <- c(input$dplyr_q1, input$dplyr_q2, input$dplyr_q3)
    score <- sum(user_answers == correct_answers)
    
    output$dplyr_quiz_feedback <- renderUI({
      tagList(
        h4(paste("得分：", score, "/ 3")),
        if(score == 3) {
          div(class = "alert alert-success",
              "恭喜你全部答对！你已经很好地掌握了dplyr包的基本函数。")
        } else {
          div(class = "alert alert-warning",
              "部分答对。以下是正确答案和解释：",
              tags$ul(
                tags$li("select()用于选择列，filter()用于筛选行，mutate()用于创建新变量。"),
                tags$li("arrange()用于排序，不是本题的正确答案。")
              ))
        }
      )
    })
  })
  
  observeEvent(input$submit_tidyr_quiz, {
    correct_answers <- c("pivot_longer()", "pivot_wider()", "separate()")
    user_answers <- c(input$tidyr_q1, input$tidyr_q2, input$tidyr_q3)
    score <- sum(user_answers == correct_answers)
    
    output$tidyr_quiz_feedback <- renderUI({
      tagList(
        h4(paste("得分：", score, "/ 3")),
        if(score == 3) {
          div(class = "alert alert-success",
              "恭喜你全部答对！你已经很好地掌握了tidyr包的基本函数。")
        } else {
          div(class = "alert alert-warning",
              "部分答对。以下是正确答案和解释：",
              tags$ul(
                tags$li("pivot_longer()用于将宽格式转换为长格式。"),
                tags$li("pivot_wider()用于将长格式转换为宽格式。"),
                tags$li("separate()用于拆分列，unite()用于合并列。")
              ))
        }
      )
    })
  })
  
  observeEvent(input$submit_ggplot2_quiz, {
    correct_answers <- c("geom_point()", "geom_line()", "geom_bar()")
    user_answers <- c(input$ggplot2_q1, input$ggplot2_q2, input$ggplot2_q3)
    score <- sum(user_answers == correct_answers)
    
    output$ggplot2_quiz_feedback <- renderUI({
      tagList(
        h4(paste("得分：", score, "/ 3")),
        if(score == 3) {
          div(class = "alert alert-success",
              "恭喜你全部答对！你已经很好地掌握了ggplot2包的基本图形类型。")
        } else {
          div(class = "alert alert-warning",
              "部分答对。以下是正确答案和解释：",
              tags$ul(
                tags$li("geom_point()用于创建散点图。"),
                tags$li("geom_line()用于创建线图。"),
                tags$li("geom_bar()用于创建条形图，geom_boxplot()用于创建箱线图。")
              ))
        }
      )
    })
  })
  
  # 提交代码反馈
  observeEvent(input$submit_dplyr_code, {
    correct_code <- 'penguins %>%
  group_by(species) %>%
  summarise(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE)
  )'
    
    output$dplyr_code_feedback <- renderUI({
      if(input$dplyr_code == correct_code) {
        div(class = "alert alert-success",
            "恭喜你答对了！你的代码完全正确。",
            tags$p("这个练习考察了以下知识点："),
            tags$ul(
              tags$li("使用group_by()进行分组"),
              tags$li("使用summarise()计算汇总统计量"),
              tags$li("处理缺失值(na.rm = TRUE)")
            ))
      } else {
        div(class = "alert alert-warning",
            "答案不完全正确。以下是正确答案和解释：",
            tags$pre(correct_code),
            tags$p("这个练习考察了以下知识点："),
            tags$ul(
              tags$li("使用group_by()进行分组"),
              tags$li("使用summarise()计算汇总统计量"),
              tags$li("处理缺失值(na.rm = TRUE)")
            ))
      }
    })
  })
  
  observeEvent(input$submit_tidyr_code, {
    correct_code <- 'penguins %>%
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm),
    names_to = "measurement",
    values_to = "value"
  )'
    
    output$tidyr_code_feedback <- renderUI({
      if(input$tidyr_code == correct_code) {
        div(class = "alert alert-success",
            "恭喜你答对了！你的代码完全正确。",
            tags$p("这个练习考察了以下知识点："),
            tags$ul(
              tags$li("使用pivot_longer()进行长宽转换"),
              tags$li("指定要转换的列(cols参数)"),
              tags$li("设置新列名(names_to和values_to参数)")
            ))
      } else {
        div(class = "alert alert-warning",
            "答案不完全正确。以下是正确答案和解释：",
            tags$pre(correct_code),
            tags$p("这个练习考察了以下知识点："),
            tags$ul(
              tags$li("使用pivot_longer()进行长宽转换"),
              tags$li("指定要转换的列(cols参数)"),
              tags$li("设置新列名(names_to和values_to参数)")
            ))
      }
    })
  })
  
  observeEvent(input$submit_ggplot2_code, {
    correct_code <- 'ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()'
    
    output$ggplot2_code_feedback <- renderUI({
      if(input$ggplot2_code == correct_code) {
        div(class = "alert alert-success",
            "恭喜你答对了！你的代码完全正确。",
            tags$p("这个练习考察了以下知识点："),
            tags$ul(
              tags$li("使用ggplot()创建图形"),
              tags$li("使用aes()设置映射"),
              tags$li("使用geom_point()创建散点图"),
              tags$li("使用color参数设置颜色")
            ))
      } else {
        div(class = "alert alert-warning",
            "答案不完全正确。以下是正确答案和解释：",
            tags$pre(correct_code),
            tags$p("这个练习考察了以下知识点："),
            tags$ul(
              tags$li("使用ggplot()创建图形"),
              tags$li("使用aes()设置映射"),
              tags$li("使用geom_point()创建散点图"),
              tags$li("使用color参数设置颜色")
            ))
      }
    })
  })
}

# 运行应用
shinyApp(ui, server) 