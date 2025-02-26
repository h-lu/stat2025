library(shiny)
library(ggplot2)
library(pwr)
library(tidyr)
library(dplyr)

# UI定义
ui <- fluidPage(
  titlePanel("检验功效分析演示"),
  
  sidebarLayout(
    sidebarPanel(
      # 输入控件
      selectInput("test_type", "检验类型:",
                  choices = c("单样本t检验" = "one.sample",
                            "独立样本t检验" = "two.sample",
                            "配对样本t检验" = "paired"),
                  selected = "one.sample"),
      
      numericInput("effect_size", "效应量 (Cohen's d):",
                  value = 0.5, min = 0.1, max = 2, step = 0.1),
      
      sliderInput("alpha", "显著性水平 (α):",
                  min = 0.01, max = 0.1, value = 0.05, step = 0.01),
      
      sliderInput("power", "期望检验功效 (1-β):",
                  min = 0.6, max = 0.99, value = 0.8, step = 0.01),
      
      # 添加说明文字
      hr(),
      helpText("本演示展示了检验功效分析的主要组成部分："),
      helpText("1. 效应量 (Effect Size): 实际效应的大小"),
      helpText("2. 显著性水平 (α): Type I错误率"),
      helpText("3. 检验功效 (1-β): 正确拒绝H₀的概率"),
      helpText("4. 样本量 (n): 达到期望功效所需的样本量")
    ),
    
    mainPanel(
      # 输出图形
      plotOutput("power_plot", height = "400px"),
      
      hr(),
      
      # 数值结果
      h4("功效分析结果："),
      verbatimTextOutput("power_results"),
      
      # 效应量解释
      h4("效应量参考值："),
      verbatimTextOutput("effect_size_reference"),
      
      # 解释
      h4("结果解释："),
      textOutput("interpretation")
    )
  )
)

# 服务器逻辑
server <- function(input, output) {
  
  # 计算所需样本量
  sample_size_calc <- reactive({
    # 使用pwr包计算样本量
    result <- switch(input$test_type,
      "one.sample" = pwr.t.test(d = input$effect_size,
                               sig.level = input$alpha,
                               power = input$power,
                               type = "one.sample",
                               alternative = "two.sided"),
      "two.sample" = pwr.t.test(d = input$effect_size,
                               sig.level = input$alpha,
                               power = input$power,
                               type = "two.sample",
                               alternative = "two.sided"),
      "paired" = pwr.t.test(d = input$effect_size,
                           sig.level = input$alpha,
                           power = input$power,
                           type = "paired",
                           alternative = "two.sided")
    )
    
    result
  })
  
  # 绘制功效曲线
  output$power_plot <- renderPlot({
    # 生成不同样本量的功效值
    n_range <- seq(5, ceiling(sample_size_calc()$n) * 2, length.out = 100)
    
    power_values <- sapply(n_range, function(n) {
      switch(input$test_type,
        "one.sample" = pwr.t.test(n = n,
                                 d = input$effect_size,
                                 sig.level = input$alpha,
                                 type = "one.sample",
                                 alternative = "two.sided")$power,
        "two.sample" = pwr.t.test(n = n,
                                 d = input$effect_size,
                                 sig.level = input$alpha,
                                 type = "two.sample",
                                 alternative = "two.sided")$power,
        "paired" = pwr.t.test(n = n,
                             d = input$effect_size,
                             sig.level = input$alpha,
                             type = "paired",
                             alternative = "two.sided")$power
      )
    })
    
    # 创建数据框
    df <- data.frame(
      n = n_range,
      power = power_values
    )
    
    # 绘制功效曲线
    ggplot(df, aes(x = n, y = power)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = input$power, linetype = "dashed", color = "red") +
      geom_vline(xintercept = ceiling(sample_size_calc()$n),
                 linetype = "dashed", color = "red") +
      labs(title = "检验功效曲线",
           x = "样本量 (n)",
           y = "检验功效 (1-β)") +
      theme_minimal() +
      theme(text = element_text(family = "STHeiti")) # 使用中文字体
  })
  
  # 显示功效分析结果
  output$power_results <- renderText({
    result <- sample_size_calc()
    paste0(
      "所需样本量: ", ceiling(result$n), "\n",
      "效应量 (d): ", round(input$effect_size, 3), "\n",
      "显著性水平 (α): ", input$alpha, "\n",
      "检验功效 (1-β): ", input$power
    )
  })
  
  # 显示效应量参考值
  output$effect_size_reference <- renderText({
    paste0(
      "Cohen's d效应量参考值：\n",
      "小效应: d ≈ 0.2\n",
      "中等效应: d ≈ 0.5\n",
      "大效应: d ≈ 0.8"
    )
  })
  
  # 提供解释
  output$interpretation <- renderText({
    result <- sample_size_calc()
    
    test_type_text <- switch(input$test_type,
      "one.sample" = "单样本t检验",
      "two.sample" = "独立样本t检验",
      "paired" = "配对样本t检验"
    )
    
    effect_size_text <- case_when(
      input$effect_size < 0.3 ~ "小",
      input$effect_size < 0.6 ~ "中等",
      TRUE ~ "大"
    )
    
    paste0(
      "在进行", test_type_text, "时，\n",
      "要检测", effect_size_text, "效应（d = ", round(input$effect_size, 2), "），\n",
      "在显著性水平α = ", input$alpha, "下，\n",
      "为了达到", input$power * 100, "%的检验功效，\n",
      "至少需要", ceiling(result$n), "个样本。"
    )
  })
}

# 运行应用
shinyApp(ui = ui, server = server) 