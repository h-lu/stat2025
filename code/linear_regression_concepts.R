# 设置编码
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

library(shiny)
library(ggplot2)

# 定义UI
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8")
  ),
  titlePanel("一元线性回归概念可视化"),
  
  sidebarLayout(
    sidebarPanel(
      # 数据生成参数
      sliderInput("n", "样本量:", min = 10, max = 100, value = 30),
      sliderInput("slope", "真实斜率:", min = -5, max = 5, value = 2, step = 0.1),
      sliderInput("intercept", "真实截距:", min = -5, max = 5, value = 1, step = 0.1),
      sliderInput("noise", "噪声水平:", min = 0.1, max = 5, value = 1, step = 0.1),
      
      # 重新生成数据按钮
      actionButton("regenerate", "重新生成数据"),
      
      # 选择要显示的内容
      checkboxGroupInput("show", "显示内容:",
                        choices = list(
                          "残差" = "residuals",
                          "回归线的变异性" = "reg_variability",
                          "回归系数的置信区间" = "coef_ci",
                          "预测区间" = "pred_interval",
                          "置信区间" = "conf_interval"
                        ),
                        selected = c("residuals", "conf_interval", "pred_interval")),
      
      # 预测值输入
      numericInput("pred_x", "预测点 X 值:", value = 0)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("图形", plotOutput("regressionPlot", height = "500px")),
        tabPanel("回归结果", verbatimTextOutput("regressionSummary")),
        tabPanel("概念解释", 
                 h3("统计概念解释"),
                 h4("残差"),
                 p("残差是实际观测值与模型预测值之间的差距，表示模型未能解释的变异。残差越小，拟合越好。"),
                 
                 h4("回归线的变异性"),
                 p("回归线的变异性展示了由于抽样导致的回归直线的不确定性。这种变异性会影响模型参数估计。"),
                 
                 h4("回归系数的置信区间"),
                 p("回归系数的置信区间表示模型参数(斜率和截距)的估计不确定性范围。"),
                 
                 h4("预测区间"),
                 p("预测区间是对新观测值的预测范围，包含了模型误差和个体观测的随机误差。"),
                 
                 h4("置信区间"),
                 p("模型的置信区间表示回归线在不同X值处的平均响应值的不确定性范围。")
        )
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  # 响应式数据生成
  sim_data <- reactiveVal()
  
  # 初始化数据或重新生成数据
  observeEvent(c(input$regenerate, input$n, input$slope, input$intercept, input$noise), {
    n <- input$n
    beta0 <- input$intercept
    beta1 <- input$slope
    sigma <- input$noise
    
    # 生成自变量
    x <- runif(n, -5, 5)
    
    # 生成因变量
    y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
    
    # 存储数据
    sim_data(data.frame(x = x, y = y))
  }, ignoreInit = FALSE)
  
  # 拟合线性回归模型
  model <- reactive({
    req(sim_data())
    lm(y ~ x, data = sim_data())
  })
  
  # 回归图形
  output$regressionPlot <- renderPlot({
    req(model())
    df <- sim_data()
    mod <- model()
    
    # 基础散点图
    p <- ggplot(df, aes(x = x, y = y)) + 
      geom_point(size = 3, alpha = 0.7) + 
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_bw() +
      theme(text = element_text(size = 14),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "一元线性回归模型",
           x = "自变量 (X)",
           y = "因变量 (Y)")
    
    # 获取新预测点
    newx <- input$pred_x
    
    # 添加不同的图形元素
    if ("residuals" %in% input$show) {
      # 添加残差线
      df$fitted <- predict(mod)
      p <- p + geom_segment(data = df, aes(x = x, y = fitted, xend = x, yend = y), 
                          color = "red", linetype = "dashed")
    }
    
    # 添加置信区间
    if ("conf_interval" %in% input$show) {
      pred_data <- data.frame(x = seq(min(df$x), max(df$x), length.out = 100))
      pred_data$fit <- predict(mod, newdata = pred_data)
      ci <- predict(mod, newdata = pred_data, interval = "confidence", level = 0.95)
      pred_data$lwr <- ci[, "lwr"]
      pred_data$upr <- ci[, "upr"]
      
      p <- p + geom_ribbon(data = pred_data, aes(x = x, y = fit, ymin = lwr, ymax = upr), 
                         alpha = 0.2, fill = "blue")
    }
    
    # 添加预测区间
    if ("pred_interval" %in% input$show) {
      pred_data <- data.frame(x = seq(min(df$x), max(df$x), length.out = 100))
      pred_data$fit <- predict(mod, newdata = pred_data)
      pi <- predict(mod, newdata = pred_data, interval = "prediction", level = 0.95)
      pred_data$lwr <- pi[, "lwr"]
      pred_data$upr <- pi[, "upr"]
      
      p <- p + geom_ribbon(data = pred_data, aes(x = x, y = fit, ymin = lwr, ymax = upr), 
                         alpha = 0.1, fill = "green")
    }
    
    # 显示预测点及其区间
    if (newx >= min(df$x) && newx <= max(df$x)) {
      newdata <- data.frame(x = newx)
      pred_y <- predict(mod, newdata = newdata)
      pred_ci <- predict(mod, newdata = newdata, interval = "confidence", level = 0.95)
      pred_pi <- predict(mod, newdata = newdata, interval = "prediction", level = 0.95)
      
      p <- p + annotate("point", x = newx, y = pred_y, color = "purple", size = 4)
      
      if ("conf_interval" %in% input$show) {
        p <- p + geom_errorbar(aes(x = newx, ymin = pred_ci[, "lwr"], ymax = pred_ci[, "upr"]), 
                             width = 0.2, color = "blue", size = 1, inherit.aes = FALSE)
      }
      
      if ("pred_interval" %in% input$show) {
        p <- p + geom_errorbar(aes(x = newx, ymin = pred_pi[, "lwr"], ymax = pred_pi[, "upr"]), 
                             width = 0.3, color = "green", size = 1, inherit.aes = FALSE)
      }
    }
    
    # 显示回归系数的置信区间
    if ("coef_ci" %in% input$show) {
      conf <- confint(mod)
      intercept <- coef(mod)[1]
      slope <- coef(mod)[2]
      intercept_lwr <- conf[1, 1]
      intercept_upr <- conf[1, 2]
      slope_lwr <- conf[2, 1]
      slope_upr <- conf[2, 2]
      
      # 在图形中添加注释
      text_x <- min(df$x) + 0.1 * (max(df$x) - min(df$x))
      text_y <- max(df$y) - 0.1 * (max(df$y) - min(df$y))
      
      coef_text <- sprintf("斜率: %.3f [%.3f, %.3f]\n截距: %.3f [%.3f, %.3f]", 
                          slope, slope_lwr, slope_upr,
                          intercept, intercept_lwr, intercept_upr)
      
      p <- p + annotate("text", x = text_x, y = text_y, 
                      label = coef_text, hjust = 0, size = 5)
    }
    
    # 展示回归线的变异性
    if ("reg_variability" %in% input$show) {
      # 模拟多条可能的回归线
      set.seed(123)
      for (i in 1:15) {
        # 使用bootstrap重采样来模拟回归线的变异性
        idx <- sample(1:nrow(df), nrow(df), replace = TRUE)
        boot_df <- df[idx, ]
        boot_mod <- lm(y ~ x, data = boot_df)
        
        # 生成预测数据
        grid_x <- seq(min(df$x), max(df$x), length.out = 100)
        grid_df <- data.frame(x = grid_x)
        grid_df$y <- predict(boot_mod, newdata = grid_df)
        
        # 添加线条
        p <- p + geom_line(data = grid_df, aes(x = x, y = y), 
                         color = "grey70", alpha = 0.3, inherit.aes = FALSE)
      }
    }
    
    return(p)
  })
  
  # 回归结果摘要
  output$regressionSummary <- renderPrint({
    req(model())
    summary(model())
  })
}

# 运行应用
shinyApp(ui = ui, server = server) 