library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# UI定义
ui <- fluidPage(
  titlePanel("假设检验错误类型演示"),
  
  sidebarLayout(
    sidebarPanel(
      # 输入控件
      numericInput("h0_mean", "原假设均值 (H₀):",
                  value = 0, step = 0.1),
      
      numericInput("h1_mean", "实际总体均值:",
                  value = 0.5, step = 0.1),
      
      numericInput("population_sd", "总体标准差 (σ):",
                  value = 1, step = 0.1),
      
      sliderInput("alpha", "显著性水平 (α):",
                  min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      
      sliderInput("sample_size", "样本量 (n):",
                  min = 10, max = 100, value = 30),
      
      # 添加说明文字
      hr(),
      helpText("本演示展示了假设检验中的两类错误："),
      helpText("Type I错误 (α): 错误地拒绝真实的H₀"),
      helpText("Type II错误 (β): 错误地接受错误的H₀"),
      helpText("检验功效 = 1 - β")
    ),
    
    mainPanel(
      # 输出图形
      plotOutput("error_plot", height = "400px"),
      
      hr(),
      
      # 数值结果
      h4("检验结果："),
      verbatimTextOutput("test_results"),
      
      # 解释
      h4("结果解释："),
      textOutput("interpretation")
    )
  )
)

# 服务器逻辑
server <- function(input, output) {
  
  # 计算检验功效和错误率
  power_calc <- reactive({
    # 计算临界值
    se <- input$population_sd / sqrt(input$sample_size)
    z_crit <- qnorm(1 - input$alpha/2) # 双尾检验
    crit_value <- input$h0_mean + z_crit * se
    
    # 计算Type II错误率(β)和检验功效(1-β)
    beta <- pnorm(crit_value, mean = input$h1_mean, 
                 sd = se) - pnorm(-crit_value, mean = input$h1_mean, sd = se)
    power <- 1 - beta
    
    list(
      crit_value = crit_value,
      beta = beta,
      power = power,
      se = se
    )
  })
  
  # 绘制分布图和错误区域
  output$error_plot <- renderPlot({
    pc <- power_calc()
    
    # 生成x值范围
    x <- seq(input$h0_mean - 4*input$population_sd/sqrt(input$sample_size),
             input$h0_mean + 4*input$population_sd/sqrt(input$sample_size),
             length.out = 1000)
    
    # 创建数据框
    df <- data.frame(
      x = rep(x, 2),
      y = c(dnorm(x, input$h0_mean, pc$se),
            dnorm(x, input$h1_mean, pc$se)),
      Distribution = rep(c("H₀分布", "实际分布"), each = length(x))
    )
    
    # 绘制图形
    ggplot(df, aes(x = x, y = y, color = Distribution)) +
      geom_line() +
      geom_vline(xintercept = c(-pc$crit_value, pc$crit_value),
                 linetype = "dashed", color = "red") +
      geom_vline(xintercept = c(input$h0_mean, input$h1_mean),
                 linetype = "dotted", color = c("blue", "green")) +
      labs(title = "假设检验中的Type I和Type II错误",
           x = "样本均值",
           y = "密度") +
      theme_minimal() +
      theme(text = element_text(family = "STHeiti")) # 使用中文字体
  })
  
  # 显示检验结果
  output$test_results <- renderText({
    pc <- power_calc()
    paste0(
      "Type I错误率 (α): ", round(input$alpha, 4), "\n",
      "Type II错误率 (β): ", round(pc$beta, 4), "\n",
      "检验功效 (1-β): ", round(pc$power, 4), "\n",
      "标准误: ", round(pc$se, 4)
    )
  })
  
  # 提供解释
  output$interpretation <- renderText({
    pc <- power_calc()
    paste0(
      "在当前设置下：\n",
      "1. 如果H₀为真（总体均值确实为", input$h0_mean, "），",
      "我们有", input$alpha * 100, "%的概率错误地拒绝它（Type I错误）。\n",
      "2. 如果H₁为真（总体均值实际为", input$h1_mean, "），",
      "我们有", round(pc$beta * 100, 1), "%的概率错误地接受H₀（Type II错误）。\n",
      "3. 检验功效为", round(pc$power * 100, 1), 
      "%，表示当H₁为真时，正确拒绝H₀的概率。"
    )
  })
}

# 运行应用
shinyApp(ui = ui, server = server) 