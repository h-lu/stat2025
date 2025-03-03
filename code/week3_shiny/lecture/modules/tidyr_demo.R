# tidyr数据整理演示模块
# 包含pivot_longer, pivot_wider, separate, unite等函数的交互式演示

library(shiny)
library(DT)

# tidyr演示UI组件
tidyrDemoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("tidyr数据整理演示"),
        tabsetPanel(
          # pivot_longer演示面板
          tabPanel("pivot_longer函数", 
            fluidRow(
              column(6,
                h4("函数说明"),
                p("pivot_longer()函数用于将宽格式数据转换为长格式数据，将列名转换为变量值。"),
                code("pivot_longer(cols = c(...), names_to = \"...\", values_to = \"...\")"),
                hr(),
                actionButton(ns("run_longer"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("longer_code")),
                h4("转换前（宽格式）"),
                DTOutput(ns("wide_data")),
                h4("转换后（长格式）"),
                DTOutput(ns("long_data"))
              )
            )
          ),
          
          # pivot_wider演示面板
          tabPanel("pivot_wider函数",
            fluidRow(
              column(6,
                h4("函数说明"),
                p("pivot_wider()函数用于将长格式数据转换为宽格式数据，将变量值转换为列名。"),
                code("pivot_wider(names_from = \"...\", values_from = \"...\")"),
                hr(),
                actionButton(ns("run_wider"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("wider_code")),
                h4("转换前（长格式）"),
                DTOutput(ns("long_data_before")),
                h4("转换后（宽格式）"),
                DTOutput(ns("wide_data_after"))
              )
            )
          ),
          
          # separate演示面板
          tabPanel("separate函数",
            fluidRow(
              column(6,
                h4("函数说明"),
                p("separate()函数用于将一列拆分为多列，通常基于某个分隔符。"),
                code("separate(col = ..., into = c(...), sep = \"...\")"),
                hr(),
                actionButton(ns("run_separate"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("separate_code")),
                h4("拆分前"),
                DTOutput(ns("before_separate")),
                h4("拆分后"),
                DTOutput(ns("after_separate"))
              )
            )
          ),
          
          # unite演示面板
          tabPanel("unite函数",
            fluidRow(
              column(6,
                h4("函数说明"),
                p("unite()函数用于将多列合并为一列，通常使用某个分隔符连接。"),
                code("unite(col = \"...\", c(...), sep = \"...\")"),
                hr(),
                actionButton(ns("run_unite"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("unite_code")),
                h4("合并前"),
                DTOutput(ns("before_unite")),
                h4("合并后"),
                DTOutput(ns("after_unite"))
              )
            )
          )
        )
      )
    )
  )
}

# tidyr演示服务器组件
tidyrDemoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 加载数据
    pivot_data <- reactive({
      source("modules/data_processing.R")
      create_pivot_example()
    })
    
    # 创建日期数据示例
    date_data <- reactive({
      data.frame(
        日期 = c("2023-10-26", "2023-10-27", "2023-10-28"),
        value = c(100, 120, 110)
      )
    })
    
    # 创建姓名数据示例
    name_data <- reactive({
      data.frame(
        姓 = c("张", "李", "王"),
        名 = c("三", "四", "五"),
        成绩 = c(85, 92, 78)
      )
    })
    
    # pivot_longer示例
    observeEvent(input$run_longer, {
      output$longer_code <- renderText({
        'wide_data %>%\n  pivot_longer(\n    cols = c(语文, 数学, 英语),\n    names_to = "科目",\n    values_to = "成绩"\n  )'
      })
      
      output$wide_data <- renderDT({
        pivot_data()$wide_data
      })
      
      output$long_data <- renderDT({
        pivot_data()$long_data
      })
    })
    
    # pivot_wider示例
    observeEvent(input$run_wider, {
      output$wider_code <- renderText({
        'long_data %>%\n  pivot_wider(\n    names_from = "科目",\n    values_from = "成绩"\n  )'
      })
      
      output$long_data_before <- renderDT({
        pivot_data()$long_data
      })
      
      output$wide_data_after <- renderDT({
        pivot_data()$long_data %>%
          pivot_wider(
            names_from = "科目",
            values_from = "成绩"
          )
      })
    })
    
    # separate示例
    observeEvent(input$run_separate, {
      output$separate_code <- renderText({
        'date_data %>%\n  separate(\n    col = 日期,\n    into = c("年份", "月份", "日期"),\n    sep = "-"\n  )'
      })
      
      output$before_separate <- renderDT({
        date_data()
      })
      
      output$after_separate <- renderDT({
        date_data() %>%
          separate(
            col = 日期,
            into = c("年份", "月份", "日期"),
            sep = "-"
          )
      })
    })
    
    # unite示例
    observeEvent(input$run_unite, {
      output$unite_code <- renderText({
        'name_data %>%\n  unite(\n    col = "姓名",\n    c(姓, 名),\n    sep = ""\n  )'
      })
      
      output$before_unite <- renderDT({
        name_data()
      })
      
      output$after_unite <- renderDT({
        name_data() %>%
          unite(
            col = "姓名",
            c(姓, 名),
            sep = ""
          )
      })
    })
  })
} 