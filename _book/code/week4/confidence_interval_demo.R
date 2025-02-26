library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# UI定义
ui <- fluidPage(
  titlePanel("置信区间宽度影响因素演示"),
  
  sidebarLayout(
    sidebarPanel(
      # 输入控件
      sliderInput("sample_size", "样本量 (n):",
                  min = 10, max = 200, value = 30),
      
      sliderInput("confidence_level", "置信水平 (1-α):",
                  min = 0.8, max = 0.99, value = 0.95, step = 0.01),
      
      sliderInput("population_sd", "总体标准差 (σ):",
                  min = 0.5, max = 5, value = 1, step = 0.1),
      
      # 添加说明文字
      hr(),
      helpText("本演示展示了影响置信区间宽度的三个主要因素："),
      helpText("1. 样本量 (n)：样本量越大，置信区间越窄"),
      helpText("2. 置信水平 (1-α)：置信水平越高，置信区间越宽"),
      helpText("3. 总体标准差 (σ)：标准差越大，置信区间越宽")
    ),
    
    mainPanel(
      # 输出图形和解释
      plotOutput("ci_plot"),
      
      hr(),
      
      # 数值结果展示
      h4("置信区间计算结果："),
      verbatimTextOutput("ci_results"),
      
      # 解释性文字
      h4("结果解释："),
      textOutput("interpretation")
    )
  )
)

# 服务器逻辑
server <- function(input, output) {
  
  # 生成模拟数据和计算置信区间
  ci_data <- reactive({
    # 设定随机种子以保持一致性
    set.seed(123)
    
    # 从正态分布生成数据
    x <- rnorm(input$sample_size, mean = 0, sd = input$population_sd)
    
    # 计算样本均值和标准误
    mean_x <- mean(x)
    se <- input$population_sd / sqrt(input$sample_size)
    
    # 计算置信区间
    alpha <- 1 - input$confidence_level
    z_score <- qnorm(1 - alpha/2)
    ci_width <- z_score * se
    
    list(
      mean = mean_x,
      lower = mean_x - ci_width,
      upper = mean_x + ci_width,
      width = 2 * ci_width,
      data = x
    )
  })
  
  # 绘制置信区间图形
  output$ci_plot <- renderPlot({
    ci <- ci_data()
    
    # 创建数据分布图
    ggplot(data.frame(x = ci$data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      geom_density(color = "blue") +
      geom_vline(xintercept = ci$mean, color = "red", linetype = "dashed") +
      geom_vline(xintercept = c(ci$lower, ci$upper), color = "darkred", linetype = "dotted") +
      labs(title = "数据分布与置信区间",
           subtitle = paste0(input$confidence_level * 100, "% 置信区间"),
           x = "值",
           y = "密度") +
      theme_minimal() +
      theme(text = element_text(family = "STHeiti")) # 使用中文字体
  })
  
  # 显示数值结果
  output$ci_results <- renderText({
    ci <- ci_data()
    paste0(
      "样本均值: ", round(ci$mean, 4), "\n",
      "置信区间: [", round(ci$lower, 4), ", ", round(ci$upper, 4), "]\n",
      "置信区间宽度: ", round(ci$width, 4), "\n",
      "标准误: ", round(input$population_sd / sqrt(input$sample_size), 4)
    )
  })
  
  # 提供解释
  output$interpretation <- renderText({
    ci <- ci_data()
    paste0(
      "当前设置下，在", input$confidence_level * 100, "%的置信水平下，",
      "样本量为", input$sample_size, "，总体标准差为", input$population_sd, "时，",
      "置信区间的宽度为", round(ci$width, 4), "。\n",
      "这表明我们有", input$confidence_level * 100, "%的把握认为总体均值落在这个区间内。"
    )
  })
}

# 运行应用
shinyApp(ui = ui, server = server) 