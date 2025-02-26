library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# UI定义
ui <- fluidPage(
  titlePanel("t检验演示"),
  
  sidebarLayout(
    sidebarPanel(
      # 输入控件
      selectInput("test_type", "检验类型:",
                  choices = c("单样本t检验" = "one.sample",
                            "独立样本t检验" = "two.sample",
                            "配对样本t检验" = "paired"),
                  selected = "one.sample"),
      
      # 单样本t检验参数
      conditionalPanel(
        condition = "input.test_type == 'one.sample'",
        numericInput("h0_mean", "原假设均值 (H₀):",
                    value = 0, step = 0.1),
        numericInput("sample_mean", "样本均值:",
                    value = 0.5, step = 0.1),
        numericInput("sample_sd", "样本标准差:",
                    value = 1, step = 0.1),
        numericInput("sample_size", "样本量:",
                    value = 30, min = 2)
      ),
      
      # 独立样本t检验参数
      conditionalPanel(
        condition = "input.test_type == 'two.sample'",
        numericInput("group1_mean", "组1均值:",
                    value = 0, step = 0.1),
        numericInput("group1_sd", "组1标准差:",
                    value = 1, step = 0.1),
        numericInput("group1_size", "组1样本量:",
                    value = 30, min = 2),
        numericInput("group2_mean", "组2均值:",
                    value = 0.5, step = 0.1),
        numericInput("group2_sd", "组2标准差:",
                    value = 1, step = 0.1),
        numericInput("group2_size", "组2样本量:",
                    value = 30, min = 2),
        checkboxInput("var_equal", "假设方差相等", TRUE)
      ),
      
      # 配对样本t检验参数
      conditionalPanel(
        condition = "input.test_type == 'paired'",
        numericInput("paired_diff_mean", "配对差异均值:",
                    value = 0.5, step = 0.1),
        numericInput("paired_diff_sd", "配对差异标准差:",
                    value = 1, step = 0.1),
        numericInput("paired_size", "配对样本量:",
                    value = 30, min = 2)
      ),
      
      # 通用参数
      selectInput("alternative", "备择假设:",
                  choices = c("双尾" = "two.sided",
                            "右尾" = "greater",
                            "左尾" = "less"),
                  selected = "two.sided"),
      
      numericInput("alpha", "显著性水平 (α):",
                  value = 0.05, min = 0.01, max = 0.1, step = 0.01),
      
      # 添加说明文字
      hr(),
      helpText("本演示展示了三种t检验："),
      helpText("1. 单样本t检验：检验一个样本的均值是否等于某个特定值"),
      helpText("2. 独立样本t检验：比较两个独立样本的均值是否相等"),
      helpText("3. 配对样本t检验：检验配对样本的差异是否显著")
    ),
    
    mainPanel(
      # 输出图形
      plotOutput("t_test_plot", height = "400px"),
      
      hr(),
      
      # 检验结果
      h4("t检验结果："),
      verbatimTextOutput("t_test_results"),
      
      # 效应量
      h4("效应量："),
      verbatimTextOutput("effect_size"),
      
      # 解释
      h4("结果解释："),
      textOutput("interpretation")
    )
  )
)

# 服务器逻辑
server <- function(input, output) {
  
  # 执行t检验
  t_test_results <- reactive({
    if (input$test_type == "one.sample") {
      # 生成样本数据
      set.seed(123)
      x <- rnorm(input$sample_size, input$sample_mean, input$sample_sd)
      
      # 执行单样本t检验
      test <- t.test(x,
                     mu = input$h0_mean,
                     alternative = input$alternative)
      
      # 计算效应量 (Cohen's d)
      d <- (mean(x) - input$h0_mean) / sd(x)
      
      list(test = test, d = d, data = list(x = x))
      
    } else if (input$test_type == "two.sample") {
      # 生成两组样本数据
      set.seed(123)
      x <- rnorm(input$group1_size, input$group1_mean, input$group1_sd)
      y <- rnorm(input$group2_size, input$group2_mean, input$group2_sd)
      
      # 执行独立样本t检验
      test <- t.test(x, y,
                     var.equal = input$var_equal,
                     alternative = input$alternative)
      
      # 计算效应量 (Cohen's d)
      pooled_sd <- sqrt(((input$group1_size-1)*var(x) + 
                        (input$group2_size-1)*var(y)) /
                       (input$group1_size + input$group2_size - 2))
      d <- (mean(x) - mean(y)) / pooled_sd
      
      list(test = test, d = d, data = list(x = x, y = y))
      
    } else { # paired
      # 生成配对样本数据
      set.seed(123)
      diff <- rnorm(input$paired_size, 
                    input$paired_diff_mean, 
                    input$paired_diff_sd)
      x <- rnorm(input$paired_size, 0, 1)
      y <- x + diff
      
      # 执行配对样本t检验
      test <- t.test(x, y,
                     paired = TRUE,
                     alternative = input$alternative)
      
      # 计算效应量 (Cohen's d)
      d <- mean(diff) / sd(diff)
      
      list(test = test, d = d, data = list(x = x, y = y, diff = diff))
    }
  })
  
  # 绘制检验结果图
  output$t_test_plot <- renderPlot({
    results <- t_test_results()
    
    if (input$test_type == "one.sample") {
      # 单样本t检验图
      data <- data.frame(x = results$data$x)
      
      ggplot(data, aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30,
                      fill = "lightblue", color = "black") +
        geom_density(color = "blue") +
        geom_vline(xintercept = input$h0_mean,
                   color = "red", linetype = "dashed") +
        geom_vline(xintercept = mean(results$data$x),
                   color = "blue", linetype = "dashed") +
        labs(title = "单样本t检验",
             subtitle = paste("H₀: μ =", input$h0_mean),
             x = "值",
             y = "密度") +
        theme_minimal() +
        theme(text = element_text(family = "STHeiti"))
      
    } else if (input$test_type == "two.sample") {
      # 独立样本t检验图
      data <- data.frame(
        value = c(results$data$x, results$data$y),
        group = factor(rep(c("组1", "组2"),
                         c(length(results$data$x), length(results$data$y))))
      )
      
      ggplot(data, aes(x = value, fill = group)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = c(mean(results$data$x), mean(results$data$y)),
                   color = c("red", "blue"), linetype = "dashed") +
        labs(title = "独立样本t检验",
             subtitle = "两组数据分布比较",
             x = "值",
             y = "密度") +
        theme_minimal() +
        theme(text = element_text(family = "STHeiti"))
      
    } else {
      # 配对样本t检验图
      data <- data.frame(diff = results$data$diff)
      
      ggplot(data, aes(x = diff)) +
        geom_histogram(aes(y = ..density..), bins = 30,
                      fill = "lightblue", color = "black") +
        geom_density(color = "blue") +
        geom_vline(xintercept = 0,
                   color = "red", linetype = "dashed") +
        geom_vline(xintercept = mean(results$data$diff),
                   color = "blue", linetype = "dashed") +
        labs(title = "配对样本t检验",
             subtitle = "配对差异分布",
             x = "差异",
             y = "密度") +
        theme_minimal() +
        theme(text = element_text(family = "STHeiti"))
    }
  })
  
  # 显示检验结果
  output$t_test_results <- renderText({
    results <- t_test_results()
    test <- results$test
    
    paste0(
      "t统计量: ", round(test$statistic, 4), "\n",
      "自由度: ", round(test$parameter, 4), "\n",
      "p值: ", round(test$p.value, 4), "\n",
      "置信区间: [", 
      round(test$conf.int[1], 4), ", ",
      round(test$conf.int[2], 4), "]\n"
    )
  })
  
  # 显示效应量
  output$effect_size <- renderText({
    results <- t_test_results()
    d <- results$d
    
    effect_size_text <- case_when(
      abs(d) < 0.2 ~ "可以忽略的",
      abs(d) < 0.5 ~ "小",
      abs(d) < 0.8 ~ "中等",
      TRUE ~ "大"
    )
    
    paste0(
      "Cohen's d = ", round(d, 4), "\n",
      "这是一个", effect_size_text, "效应量"
    )
  })
  
  # 提供解释
  output$interpretation <- renderText({
    results <- t_test_results()
    test <- results$test
    
    # 根据p值判断结果
    decision <- if(test$p.value < input$alpha) {
      "拒绝原假设"
    } else {
      "不能拒绝原假设"
    }
    
    # 根据检验类型提供不同的解释
    if (input$test_type == "one.sample") {
      paste0(
        "在α = ", input$alpha, " 的显著性水平下，",
        "我们", decision, "。\n",
        if(test$p.value < input$alpha) {
          paste0("有充分证据表明总体均值与", input$h0_mean, "存在显著差异。")
        } else {
          paste0("没有充分证据表明总体均值与", input$h0_mean, "存在显著差异。")
        }
      )
    } else if (input$test_type == "two.sample") {
      paste0(
        "在α = ", input$alpha, " 的显著性水平下，",
        "我们", decision, "。\n",
        if(test$p.value < input$alpha) {
          "有充分证据表明两组均值存在显著差异。"
        } else {
          "没有充分证据表明两组均值存在显著差异。"
        }
      )
    } else {
      paste0(
        "在α = ", input$alpha, " 的显著性水平下，",
        "我们", decision, "。\n",
        if(test$p.value < input$alpha) {
          "有充分证据表明配对样本的差异显著不为零。"
        } else {
          "没有充分证据表明配对样本存在显著差异。"
        }
      )
    }
  })
}

# 运行应用
shinyApp(ui = ui, server = server) 