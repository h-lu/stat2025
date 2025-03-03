# 第三周：描述性统计 - 数据探索与可视化
# 教师课堂演示用交互式课件

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(palmerpenguins)

# 加载知识图谱函数
source("knowledge_map.R")

# 加载模块
source("modules/data_processing.R")
source("modules/dplyr_demo.R")
source("modules/tidyr_demo.R")
source("modules/ggplot2_demo.R")

# 定义UI
ui <- dashboardPage(
  # 仪表板标题
  dashboardHeader(title = "第三周：数据探索与可视化"),
  
  # 侧边栏
  dashboardSidebar(
    sidebarMenu(
      menuItem("课程概述", tabName = "overview", icon = icon("book")),
      menuItem("数据清洗进阶", tabName = "dplyr", icon = icon("filter")),
      menuItem("数据整理", tabName = "tidyr", icon = icon("table")),
      menuItem("数据可视化", tabName = "ggplot2", icon = icon("chart-bar")),
      menuItem("综合应用", tabName = "comprehensive", icon = icon("project-diagram"))
    )
  ),
  
  # 主体内容
  dashboardBody(
    tabItems(
      # 课程概述页面
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "本周学习目标", width = 12, status = "primary",
            tags$ul(
              tags$li("学习使用 dplyr 和 tidyr 包进行更高级的数据清洗和整理"),
              tags$li("掌握 ggplot2 包基本语法，绘制常用数据可视化图表 (直方图、散点图、箱线图)"),
              tags$li("学习美化 ggplot2 图形，添加标题、标签、修改颜色等"),
              tags$li("了解数据可视化在数据探索中的作用，通过可视化发现数据规律"),
              tags$li("综合运用 dplyr, tidyr, ggplot2 进行完整的数据探索和可视化分析")
            )
          )
        ),
        fluidRow(
          box(
            title = "数据集介绍", width = 6, status = "info",
            p("本课程使用Palmer Penguins数据集进行演示。该数据集包含了南极洲Palmer站点附近三种企鹅的测量数据。"),
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
            p("数据来源：", tags$a(href = "https://allisonhorst.github.io/palmerpenguins/", "Palmer Penguins R Package"))
          ),
          box(
            title = "数据预览", width = 6, status = "info",
            DTOutput("data_preview")
          )
        ),
        fluidRow(
          box(
            title = "课程内容", width = 12, status = "success",
            p("本课程分为三个主要部分："),
            tags$ol(
              tags$li(
                strong("数据清洗进阶："),
                "学习dplyr包的高级函数，如mutate(), arrange(), group_by(), summarise()等，用于数据的筛选、转换和汇总。"
              ),
              tags$li(
                strong("数据整理："),
                "学习tidyr包的函数，如pivot_longer(), pivot_wider(), separate(), unite()等，用于数据的重塑和整理。"
              ),
              tags$li(
                strong("数据可视化："),
                "学习ggplot2包的基本语法和常用图表类型，以及图形美化技巧，用于数据的可视化探索。"
              )
            ),
            p("每个部分都包含交互式演示，可以通过左侧菜单进行导航。")
          )
        ),
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
      
      # dplyr高级函数演示页面
      tabItem(tabName = "dplyr",
        dplyrDemoUI("dplyr_demo")
      ),
      
      # tidyr数据整理演示页面
      tabItem(tabName = "tidyr",
        tidyrDemoUI("tidyr_demo")
      ),
      
      # ggplot2数据可视化演示页面
      tabItem(tabName = "ggplot2",
        ggplot2DemoUI("ggplot2_demo")
      ),
      
      # 综合应用页面
      tabItem(tabName = "comprehensive",
        fluidRow(
          box(
            title = "综合应用案例", width = 12, status = "primary",
            p("本案例将综合运用dplyr、tidyr和ggplot2包，对Palmer Penguins数据集进行探索性分析。"),
            p("我们将探索以下问题："),
            tags$ol(
              tags$li("不同种类企鹅的体型特征有何差异？"),
              tags$li("企鹅的嘴峰长度与体重之间是否存在关系？这种关系在不同种类企鹅中是否一致？"),
              tags$li("不同岛屿上企鹅的分布情况如何？")
            )
          )
        ),
        fluidRow(
          tabBox(
            title = "分析步骤", width = 12,
            tabPanel("步骤1：数据清洗",
              fluidRow(
                column(6,
                  h4("代码"),
                  verbatimTextOutput("clean_code")
                ),
                column(6,
                  h4("结果"),
                  DTOutput("clean_result")
                )
              ),
              actionButton("run_clean", "运行数据清洗", class = "btn-primary")
            ),
            tabPanel("步骤2：数据汇总",
              fluidRow(
                column(6,
                  h4("代码"),
                  verbatimTextOutput("summary_code")
                ),
                column(6,
                  h4("结果"),
                  DTOutput("summary_result")
                )
              ),
              actionButton("run_summary", "运行数据汇总", class = "btn-primary")
            ),
            tabPanel("步骤3：数据可视化",
              fluidRow(
                column(4,
                  selectInput("viz_question", "选择分析问题：",
                    choices = c(
                      "不同种类企鹅的体型特征比较" = "q1",
                      "嘴峰长度与体重的关系" = "q2",
                      "不同岛屿上企鹅的分布" = "q3"
                    )
                  ),
                  actionButton("run_viz", "生成可视化", class = "btn-primary")
                ),
                column(8,
                  plotlyOutput("viz_result", height = "400px")
                )
              ),
              fluidRow(
                column(12,
                  h4("代码"),
                  verbatimTextOutput("viz_code")
                )
              )
            ),
            tabPanel("步骤4：结论",
              uiOutput("conclusion")
            )
          )
        )
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  # 加载数据
  penguins_data <- reactive({
    load_penguins_data()
  })
  
  # 数据预览
  output$data_preview <- renderDT({
    datatable(head(penguins_data(), 10), options = list(scrollX = TRUE))
  })
  
  # 知识图谱渲染
  output$data_analysis_flow <- renderUI({
    HTML(data_analysis_flow())
  })
  
  output$dplyr_map <- renderUI({
    HTML(dplyr_knowledge_map())
  })
  
  output$tidyr_map <- renderUI({
    HTML(tidyr_knowledge_map())
  })
  
  output$ggplot2_map <- renderUI({
    HTML(ggplot2_knowledge_map())
  })
  
  # 调用dplyr演示模块
  dplyrDemoServer("dplyr_demo")
  
  # 调用tidyr演示模块
  tidyrDemoServer("tidyr_demo")
  
  # 调用ggplot2演示模块
  ggplot2DemoServer("ggplot2_demo")
  
  # 综合应用 - 数据清洗
  observeEvent(input$run_clean, {
    clean_code <- 'penguins_clean <- penguins %>%\n  # 移除缺失值\n  drop_na() %>%\n  # 创建新变量\n  mutate(\n    body_mass_kg = body_mass_g / 1000,\n    bill_ratio = bill_length_mm / bill_depth_mm\n  )'
    
    output$clean_code <- renderText({ clean_code })
    
    penguins_clean <- penguins_data() %>%
      drop_na() %>%
      mutate(
        body_mass_kg = body_mass_g / 1000,
        bill_ratio = bill_length_mm / bill_depth_mm
      )
    
    output$clean_result <- renderDT({
      datatable(head(penguins_clean, 10), options = list(scrollX = TRUE))
    })
  })
  
  # 综合应用 - 数据汇总
  observeEvent(input$run_summary, {
    summary_code <- 'penguins_summary <- penguins_clean %>%\n  group_by(species, sex) %>%\n  summarise(\n    count = n(),\n    mean_body_mass = mean(body_mass_g),\n    mean_bill_length = mean(bill_length_mm),\n    mean_bill_depth = mean(bill_depth_mm),\n    mean_flipper_length = mean(flipper_length_mm),\n    .groups = "drop"\n  ) %>%\n  arrange(species, sex)'
    
    output$summary_code <- renderText({ summary_code })
    
    penguins_clean <- penguins_data() %>%
      drop_na() %>%
      mutate(
        body_mass_kg = body_mass_g / 1000,
        bill_ratio = bill_length_mm / bill_depth_mm
      )
    
    penguins_summary <- penguins_clean %>%
      group_by(species, sex) %>%
      summarise(
        count = n(),
        mean_body_mass = mean(body_mass_g),
        mean_bill_length = mean(bill_length_mm),
        mean_bill_depth = mean(bill_depth_mm),
        mean_flipper_length = mean(flipper_length_mm),
        .groups = "drop"
      ) %>%
      arrange(species, sex)
    
    output$summary_result <- renderDT({
      datatable(penguins_summary, options = list(scrollX = TRUE))
    })
  })
  
  # 综合应用 - 数据可视化
  observeEvent(input$run_viz, {
    penguins_clean <- penguins_data() %>%
      drop_na() %>%
      mutate(
        body_mass_kg = body_mass_g / 1000,
        bill_ratio = bill_length_mm / bill_depth_mm
      )
    
    if (input$viz_question == "q1") {
      viz_code <- 'ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +\n  geom_boxplot(alpha = 0.7) +\n  facet_wrap(~ sex) +\n  labs(\n    title = "不同种类企鹅的体重分布",\n    subtitle = "按性别分组",\n    x = "企鹅种类",\n    y = "体重 (g)"\n  ) +\n  scale_fill_brewer(palette = "Set2") +\n  theme_minimal()'
      
      p <- ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ sex) +
        labs(
          title = "不同种类企鹅的体重分布",
          subtitle = "按性别分组",
          x = "企鹅种类",
          y = "体重 (g)"
        ) +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal()
      
      output$conclusion <- renderUI({
        tagList(
          h4("结论："),
          tags$ul(
            tags$li("Gentoo企鹅的体重明显高于其他两种企鹅，无论雌性还是雄性。"),
            tags$li("Chinstrap企鹅和Adelie企鹅的体重相对接近，但Chinstrap企鹅略重。"),
            tags$li("在所有三种企鹅中，雄性的体重都高于雌性。"),
            tags$li("Gentoo企鹅的体重变异性较大，特别是雄性。")
          )
        )
      })
      
    } else if (input$viz_question == "q2") {
      viz_code <- 'ggplot(penguins_clean, aes(x = bill_length_mm, y = body_mass_g, color = species)) +\n  geom_point(alpha = 0.7, size = 3) +\n  geom_smooth(method = "lm", se = FALSE) +\n  labs(\n    title = "企鹅嘴峰长度与体重的关系",\n    subtitle = "按企鹅种类分组",\n    x = "嘴峰长度 (mm)",\n    y = "体重 (g)"\n  ) +\n  scale_color_brewer(palette = "Set2") +\n  theme_minimal()'
      
      p <- ggplot(penguins_clean, aes(x = bill_length_mm, y = body_mass_g, color = species)) +
        geom_point(alpha = 0.7, size = 3) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(
          title = "企鹅嘴峰长度与体重的关系",
          subtitle = "按企鹅种类分组",
          x = "嘴峰长度 (mm)",
          y = "体重 (g)"
        ) +
        scale_color_brewer(palette = "Set2") +
        theme_minimal()
      
      output$conclusion <- renderUI({
        tagList(
          h4("结论："),
          tags$ul(
            tags$li("在所有三种企鹅中，嘴峰长度与体重之间都存在正相关关系：嘴峰越长，体重越大。"),
            tags$li("这种相关关系在Gentoo企鹅中最为明显，斜率最大。"),
            tags$li("Adelie企鹅和Chinstrap企鹅的相关关系相对较弱。"),
            tags$li("不同种类企鹅的嘴峰长度和体重分布在不同的范围内，形成了明显的聚类。")
          )
        )
      })
      
    } else if (input$viz_question == "q3") {
      viz_code <- 'ggplot(penguins_clean, aes(x = island, fill = species)) +\n  geom_bar(position = "dodge") +\n  labs(\n    title = "不同岛屿上企鹅的分布情况",\n    x = "岛屿",\n    y = "企鹅数量"\n  ) +\n  scale_fill_brewer(palette = "Set2") +\n  theme_minimal()'
      
      p <- ggplot(penguins_clean, aes(x = island, fill = species)) +
        geom_bar(position = "dodge") +
        labs(
          title = "不同岛屿上企鹅的分布情况",
          x = "岛屿",
          y = "企鹅数量"
        ) +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal()
      
      output$conclusion <- renderUI({
        tagList(
          h4("结论："),
          tags$ul(
            tags$li("Biscoe岛上主要分布着Gentoo企鹅和少量Adelie企鹅。"),
            tags$li("Dream岛上分布着Chinstrap企鹅和Adelie企鹅。"),
            tags$li("Torgersen岛上只有Adelie企鹅。"),
            tags$li("Adelie企鹅是唯一分布在所有三个岛屿上的种类。")
          )
        )
      })
    }
    
    output$viz_code <- renderText({ viz_code })
    output$viz_result <- renderPlotly({ ggplotly(p) })
  })
}

# 运行应用
shinyApp(ui, server) 