library(shiny)
library(tidyverse)
library(broom)
library(plotly)
library(DT)

# 定义UI
ui <- navbarPage(
  title = "线性回归模型与最小二乘法",
  
  # 第一页：回归分析概述
  tabPanel(
    "回归分析概述",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("回归分析的目的与应用"),
        hr(),
        h3("什么是回归分析？"),
        p("回归分析是研究变量之间关系的统计方法，能够帮助我们："),
        tags$ul(
          tags$li("研究因变量与自变量之间的关系"),
          tags$li("构建模型进行预测和解释现象"),
          tags$li("评估变量之间关联的程度和方向")
        ),
        
        h3("回归分析的应用场景"),
        p("回归分析在商业分析中有广泛应用："),
        tags$ul(
          tags$li("预测：销售额预测、房价预测、股票价格预测等"),
          tags$li("行为分析：分析影响消费者购买行为的因素"),
          tags$li("效果评估：评估营销活动、政策实施等效果"),
          tags$li("绩效识别：识别影响企业绩效的关键指标")
        ),
        
        h3("回归分析的类型"),
        p("根据关系类型和自变量数量，回归分析可以分为："),
        tags$ul(
          tags$li("简单线性回归：一个自变量与因变量之间的线性关系"),
          tags$li("多元线性回归：多个自变量与因变量之间的线性关系"),
          tags$li("非线性回归：自变量与因变量之间的非线性关系")
        ),
        
        hr(),
        h4("在本教程中，我们将主要聚焦于简单线性回归模型，这是理解其他回归模型的基础。")
      )
    )
  ),
  
  # 第二页：简单线性回归模型
  tabPanel(
    "简单线性回归模型",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("简单线性回归模型"),
        hr(),
        h3("模型形式"),
        withMathJax(
          p("简单线性回归模型的基本形式：$$y = \\beta_0 + \\beta_1 x + \\epsilon$$")
        ),
        p("其中："),
        tags$ul(
          tags$li("\\(y\\)：因变量"),
          tags$li("\\(x\\)：自变量"),
          tags$li("\\(\\beta_0\\)：截距，表示当\\(x=0\\)时，\\(y\\)的期望值"),
          tags$li("\\(\\beta_1\\)：斜率，表示\\(x\\)每增加一个单位，\\(y\\)的平均变化量"),
          tags$li("\\(\\epsilon\\)：随机误差项，表示模型无法解释的随机变异")
        ),
        
        hr(),
        h3("交互式演示：理解截距和斜率"),
        fluidRow(
          column(
            width = 4,
            sliderInput("beta0", "调整截距 (β₀):", min = -10, max = 10, value = 5, step = 0.5),
            sliderInput("beta1", "调整斜率 (β₁):", min = -2, max = 2, value = 0.5, step = 0.1)
          ),
          column(
            width = 8,
            plotOutput("regressionPlot")
          )
        ),
        hr(),
        p("通过调整滑块，你可以看到截距和斜率的变化如何影响回归线。这有助于直观理解线性回归模型的参数含义。")
      )
    )
  ),
  
  # 第三页：最小二乘法
  tabPanel(
    "最小二乘法",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("最小二乘法"),
        hr(),
        h3("最小二乘法的核心思想"),
        p("最小二乘法(OLS)是求解回归系数的标准方法，核心思想是："),
        p("找到能使预测值与实际值之间的误差平方和最小的回归线。"),
        
        withMathJax(
          p("残差平方和(RSS)：
            $$RSS = \\sum_{i=1}^{n} (y_i - \\hat{y}_i)^2 = \\sum_{i=1}^{n} (y_i - (\\hat{\\beta}_0 + \\hat{\\beta}_1 x_i))^2$$")
        ),
        
        hr(),
        h3("交互式演示：理解最小二乘法"),
        fluidRow(
          column(
            width = 4,
            selectInput("dataset", "选择数据集:", 
                        choices = c("mtcars (汽车数据)", "faithful (间歇泉数据)"), 
                        selected = "mtcars (汽车数据)"),
            conditionalPanel(
              condition = "input.dataset == 'mtcars (汽车数据)'",
              selectInput("x_var", "选择自变量 (X):", 
                          choices = c("wt (车重)" = "wt", "hp (马力)" = "hp", "disp (排量)" = "disp"), 
                          selected = "wt")
            ),
            conditionalPanel(
              condition = "input.dataset == 'faithful (间歇泉数据)'",
              selectInput("x_var_faithful", "选择自变量 (X):", 
                          choices = c("waiting (等待时间)" = "waiting", "eruptions (喷发时间)" = "eruptions"), 
                          selected = "waiting")
            ),
            checkboxInput("show_residuals", "显示残差", TRUE),
            checkboxInput("show_rss", "显示残差平方和 (RSS)", TRUE)
          ),
          column(
            width = 8,
            plotlyOutput("olsPlot"),
            verbatimTextOutput("rssValue")
          )
        ),
        hr(),
        p("通过选择不同的数据集和变量，你可以看到最小二乘法如何找到最佳拟合直线。残差线显示了每个数据点到回归线的垂直距离。")
      )
    )
  ),
  
  # 第四页：R语言实现
  tabPanel(
    "R语言实现",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("R语言实现简单线性回归"),
        hr(),
        h3("使用lm()函数构建模型"),
        fluidRow(
          column(
            width = 4,
            selectInput("r_dataset", "选择数据集:", 
                        choices = c("mtcars (汽车数据)", "faithful (间歇泉数据)"), 
                        selected = "mtcars (汽车数据)"),
            conditionalPanel(
              condition = "input.r_dataset == 'mtcars (汽车数据)'",
              selectInput("r_x_var", "选择自变量 (X):", 
                          choices = c("wt (车重)" = "wt", "hp (马力)" = "hp", "disp (排量)" = "disp"), 
                          selected = "wt"),
              selectInput("r_y_var", "选择因变量 (Y):", 
                          choices = c("mpg (油耗)" = "mpg", "qsec (加速时间)" = "qsec"), 
                          selected = "mpg")
            ),
            conditionalPanel(
              condition = "input.r_dataset == 'faithful (间歇泉数据)'",
              selectInput("r_x_var_faithful", "选择自变量 (X):", 
                          choices = c("waiting (等待时间)" = "waiting", "eruptions (喷发时间)" = "eruptions"), 
                          selected = "waiting"),
              selectInput("r_y_var_faithful", "选择因变量 (Y):", 
                          choices = c("eruptions (喷发时间)" = "eruptions", "waiting (等待时间)" = "waiting"), 
                          selected = "eruptions")
            ),
            actionButton("run_lm", "运行回归分析", class = "btn-primary")
          ),
          column(
            width = 8,
            plotOutput("rModelPlot"),
            verbatimTextOutput("rModelSummary"),
            h4("R代码"),
            verbatimTextOutput("rCode")
          )
        ),
        hr(),
        h3("理解模型输出"),
        p("模型摘要(summary)中的关键信息:"),
        tags$ul(
          tags$li("Coefficients: 回归系数(截距和斜率)的估计值及其显著性"),
          tags$li("Multiple R-squared: 决定系数，表示模型解释的因变量方差比例"),
          tags$li("F-statistic: 整体模型显著性的检验结果")
        )
      )
    )
  ),
  
  # 第五页：模型评估
  tabPanel(
    "模型评估",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("模型评估指标"),
        hr(),
        h3("拟合优度 (R²)"),
        p("R²是衡量模型解释因变量变异程度的指标:"),
        withMathJax(
          p("$$R^2 = 1 - \\frac{SSE}{SST} = 1 - \\frac{\\sum(y_i - \\hat{y}_i)^2}{\\sum(y_i - \\bar{y})^2}$$")
        ),
        p("R²的取值范围在0到1之间，越接近1表示模型拟合越好。"),
        
        hr(),
        h3("残差分析"),
        fluidRow(
          column(
            width = 4,
            selectInput("eval_dataset", "选择数据集:", 
                        choices = c("mtcars (汽车数据)", "faithful (间歇泉数据)"), 
                        selected = "mtcars (汽车数据)"),
            conditionalPanel(
              condition = "input.eval_dataset == 'mtcars (汽车数据)'",
              selectInput("eval_x_var", "选择自变量 (X):", 
                          choices = c("wt (车重)" = "wt", "hp (马力)" = "hp", "disp (排量)" = "disp"), 
                          selected = "wt"),
              selectInput("eval_y_var", "选择因变量 (Y):", 
                          choices = c("mpg (油耗)" = "mpg", "qsec (加速时间)" = "qsec"), 
                          selected = "mpg")
            ),
            conditionalPanel(
              condition = "input.eval_dataset == 'faithful (间歇泉数据)'",
              selectInput("eval_x_var_faithful", "选择自变量 (X):", 
                          choices = c("waiting (等待时间)" = "waiting", "eruptions (喷发时间)" = "eruptions"), 
                          selected = "waiting"),
              selectInput("eval_y_var_faithful", "选择因变量 (Y):", 
                          choices = c("eruptions (喷发时间)" = "eruptions", "waiting (等待时间)" = "waiting"), 
                          selected = "eruptions")
            ),
            actionButton("run_diagnostics", "运行诊断分析", class = "btn-primary")
          ),
          column(
            width = 8,
            plotOutput("diagnosticPlots"),
            verbatimTextOutput("modelMetrics")
          )
        ),
        hr(),
        h3("诊断图解读"),
        p("残差与拟合值图：用于检查线性性和同方差性假设"),
        p("QQ图：用于检查残差的正态性假设"),
        p("通过这些诊断图，我们可以评估模型假设是否满足，从而判断模型是否可靠。")
      )
    )
  ),
  
  # 第六页：练习
  tabPanel(
    "练习",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("练习题"),
        hr(),
        
        h3("练习1: 理解回归系数"),
        p("在一个简单线性回归模型 y = 3.5 + 0.8x 中:"),
        radioButtons("q1", "当x增加1个单位时，y平均增加多少?",
                     choices = c("0.8", "3.5", "4.3", "不能确定"),
                     selected = character(0)),
        actionButton("check_q1", "检查答案", class = "btn-info"),
        verbatimTextOutput("answer_q1"),
        
        hr(),
        h3("练习2: 分析实际数据"),
        p("使用mtcars数据集，分析车重(wt)对油耗(mpg)的影响:"),
        p("首先，观察下面的散点图和回归线，然后回答问题。"),
        plotOutput("exercise_plot"),
        radioButtons("q2", "根据图表，车重与油耗之间的关系是:",
                     choices = c("正相关：车重增加，油耗增加", 
                                "负相关：车重增加，油耗减少", 
                                "没有明显相关性"),
                     selected = character(0)),
        actionButton("check_q2", "检查答案", class = "btn-info"),
        verbatimTextOutput("answer_q2"),
        
        hr(),
        h3("练习3: 模型评估"),
        p("运行下面的回归分析，然后回答问题:"),
        verbatimTextOutput("exercise_summary"),
        radioButtons("q3", "基于模型摘要，哪个陈述是正确的?",
                     choices = c("模型解释了大约75%的自变量变异",
                                "截距系数不显著",
                                "车重每增加1单位，油耗平均减少5.344单位",
                                "模型整体不显著"),
                     selected = character(0)),
        actionButton("check_q3", "检查答案", class = "btn-info"),
        verbatimTextOutput("answer_q3"),
        
        hr(),
        h3("练习4: 应用题"),
        p("某市场研究员收集了广告支出(x，单位：千元)与销售额(y，单位：万元)的数据，建立回归模型为：y = 10 + 2.5x"),
        textInput("q4", "若广告支出为5千元，预测销售额为多少万元?", ""),
        actionButton("check_q4", "检查答案", class = "btn-info"),
        verbatimTextOutput("answer_q4"),
        
        hr(),
        h3("小结"),
        p("通过以上练习，你应该能够理解简单线性回归模型的基本概念、参数解释和应用方法。"),
        p("记住，线性回归是数据分析和预测的基础工具，掌握这些核心概念将有助于你进一步学习更复杂的统计模型。")
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  
  # ==== 第二页：简单线性回归模型 ====
  # 生成随机数据点
  set.seed(123)
  x_values <- seq(0, 10, length.out = 20)
  
  output$regressionPlot <- renderPlot({
    beta0 <- input$beta0
    beta1 <- input$beta1
    
    # 生成带有随机误差的y值
    set.seed(123)  # 确保误差项一致
    y_values <- beta0 + beta1 * x_values + rnorm(length(x_values), 0, 1.5)
    
    # 创建数据框
    df <- tibble(x = x_values, y = y_values)
    
    # 使用ggplot2绘图
    ggplot(df, aes(x = x, y = y)) +
      geom_point(size = 3, color = "blue", alpha = 0.7) +
      geom_abline(intercept = beta0, slope = beta1, color = "red", size = 1.2) +
      annotate("text", x = 9, y = beta0 + beta1 * 9 + 1, 
               label = paste0("y = ", beta0, " + ", beta1, "x"), color = "red", size = 5) +
      labs(title = "简单线性回归模型",
           x = "自变量 (x)",
           y = "因变量 (y)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  # ==== 第三页：最小二乘法 ====
  get_ols_data <- reactive({
    if (input$dataset == "mtcars (汽车数据)") {
      df <- mtcars
      x_col <- input$x_var
      y_col <- "mpg"  # 默认使用mpg作为因变量
    } else if (input$dataset == "faithful (间歇泉数据)") {
      df <- faithful
      x_col <- input$x_var_faithful
      y_col <- "eruptions"  # 默认使用eruptions作为因变量
    }
    
    # 拟合模型
    model <- lm(formula(paste(y_col, "~", x_col)), data = df)
    
    # 准备数据
    df <- df %>%
      select(all_of(c(x_col, y_col))) %>%
      rename(x = 1, y = 2)
    
    # 添加预测值和残差
    df <- df %>%
      mutate(
        predicted = predict(model),
        residuals = y - predicted
      )
    
    # 计算RSS
    rss <- sum(df$residuals^2)
    
    # 返回结果
    list(
      df = df,
      model = model,
      rss = rss,
      x_lab = if(input$dataset == "mtcars (汽车数据)") input$x_var else input$x_var_faithful,
      y_lab = if(input$dataset == "mtcars (汽车数据)") "mpg (油耗)" else "eruptions (喷发时间)"
    )
  })
  
  output$olsPlot <- renderPlotly({
    data <- get_ols_data()
    df <- data$df
    model <- data$model
    x_lab <- data$x_lab
    y_lab <- data$y_lab
    
    # 基础散点图
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(size = 3, color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", size = 1.2) +
      labs(
        title = "最小二乘法拟合",
        x = x_lab,
        y = y_lab
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # 添加残差线
    if (input$show_residuals) {
      p <- p + geom_segment(aes(xend = x, yend = predicted), color = "darkgreen", alpha = 0.5)
    }
    
    ggplotly(p) %>%
      layout(title = list(text = "最小二乘法拟合", y = 0.95))
  })
  
  output$rssValue <- renderText({
    if (input$show_rss) {
      data <- get_ols_data()
      paste("残差平方和 (RSS) =", round(data$rss, 3))
    }
  })
  
  # ==== 第四页：R语言实现 ====
  get_r_data <- reactive({
    if (input$r_dataset == "mtcars (汽车数据)") {
      df <- mtcars
      x_col <- input$r_x_var
      y_col <- input$r_y_var
    } else if (input$r_dataset == "faithful (间歇泉数据)") {
      df <- faithful
      x_col <- input$r_x_var_faithful
      y_col <- input$r_y_var_faithful
    }
    
    # 返回数据框和列名
    list(
      df = df,
      x_col = x_col,
      y_col = y_col
    )
  })
  
  r_model <- eventReactive(input$run_lm, {
    data <- get_r_data()
    df <- data$df
    x_col <- data$x_col
    y_col <- data$y_col
    
    # 构建模型
    formula_text <- paste(y_col, "~", x_col)
    model <- lm(formula(formula_text), data = df)
    
    # 构建R代码
    code <- paste0(
      "# 加载必要的包\n",
      "library(tidyverse)\n\n",
      "# 查看数据集\n",
      "head(", input$r_dataset %>% str_extract("^[^ ]+"), ")\n\n",
      "# 构建线性回归模型\n",
      "model <- lm(", formula_text, ", data = ", input$r_dataset %>% str_extract("^[^ ]+"), ")\n\n",
      "# 查看模型摘要\n",
      "summary(model)\n\n",
      "# 绘制回归图\n",
      "ggplot(", input$r_dataset %>% str_extract("^[^ ]+"), ", aes(x = ", x_col, ", y = ", y_col, ")) +\n",
      "  geom_point() +\n",
      "  geom_smooth(method = \"lm\", se = TRUE) +\n",
      "  labs(title = \"回归分析: ", y_col, " ~ ", x_col, "\")"
    )
    
    list(
      model = model,
      code = code,
      df = df,
      x_col = x_col,
      y_col = y_col,
      x_lab = if(input$r_dataset == "mtcars (汽车数据)") input$r_x_var else input$r_x_var_faithful,
      y_lab = if(input$r_dataset == "mtcars (汽车数据)") input$r_y_var else input$r_y_var_faithful
    )
  })
  
  output$rModelPlot <- renderPlot({
    req(r_model())
    
    data <- r_model()
    df <- data$df
    x_col <- data$x_col
    y_col <- data$y_col
    x_lab <- data$x_lab
    y_lab <- data$y_lab
    
    # 绘制回归图
    ggplot(df, aes_string(x = x_col, y = y_col)) +
      geom_point(size = 3, color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(
        title = paste("回归分析:", y_lab, "~", x_lab),
        x = x_lab,
        y = y_lab
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  output$rModelSummary <- renderPrint({
    req(r_model())
    summary(r_model()$model)
  })
  
  output$rCode <- renderText({
    req(r_model())
    r_model()$code
  })
  
  # ==== 第五页：模型评估 ====
  eval_model <- eventReactive(input$run_diagnostics, {
    if (input$eval_dataset == "mtcars (汽车数据)") {
      df <- mtcars
      x_col <- input$eval_x_var
      y_col <- input$eval_y_var
    } else if (input$eval_dataset == "faithful (间歇泉数据)") {
      df <- faithful
      x_col <- input$eval_x_var_faithful
      y_col <- input$eval_y_var_faithful
    }
    
    # 构建模型
    formula_text <- paste(y_col, "~", x_col)
    model <- lm(formula(formula_text), data = df)
    
    # 获取模型指标
    model_metrics <- glance(model)
    
    list(
      model = model,
      metrics = model_metrics
    )
  })
  
  output$diagnosticPlots <- renderPlot({
    req(eval_model())
    par(mfrow = c(2, 2))
    plot(eval_model()$model)
  })
  
  output$modelMetrics <- renderPrint({
    req(eval_model())
    metrics <- eval_model()$metrics
    
    cat("模型评估指标：\n\n")
    cat("R² (R-squared):", round(metrics$r.squared, 4), "\n")
    cat("调整R² (Adjusted R-squared):", round(metrics$adj.r.squared, 4), "\n")
    cat("残差标准误差 (RSE):", round(metrics$sigma, 4), "\n")
    cat("F统计量:", round(metrics$statistic, 4), "\n")
    cat("p值:", format(metrics$p.value, scientific = TRUE), "\n")
    cat("AIC:", round(metrics$AIC, 4), "\n")
    cat("BIC:", round(metrics$BIC, 4), "\n")
  })
  
  # ==== 第六页：练习 ====
  # 练习1
  output$answer_q1 <- renderText({
    req(input$check_q1)
    if (input$q1 == "0.8") {
      "正确！在回归方程 y = 3.5 + 0.8x 中，0.8是斜率，表示x每增加1个单位，y平均增加0.8个单位。"
    } else {
      "不正确。在回归方程 y = 3.5 + 0.8x 中，0.8是斜率，表示x每增加1个单位，y平均增加0.8个单位。"
    }
  })
  
  # 练习2
  output$exercise_plot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point(size = 3, color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", size = 1.2) +
      labs(
        title = "汽车重量与燃油效率的关系",
        x = "重量 (wt, 1000 lbs)",
        y = "燃油效率 (mpg, 英里/加仑)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  output$answer_q2 <- renderText({
    req(input$check_q2)
    if (input$q2 == "负相关：车重增加，油耗减少") {
      "正确！图表显示车重(wt)与油耗(mpg)之间存在明显的负相关关系，即车重增加时，油耗减少。"
    } else {
      "不正确。图表显示车重(wt)与油耗(mpg)之间存在明显的负相关关系，即车重增加时，油耗减少。"
    }
  })
  
  # 练习3
  output$exercise_summary <- renderPrint({
    model <- lm(mpg ~ wt, data = mtcars)
    summary(model)
  })
  
  output$answer_q3 <- renderText({
    req(input$check_q3)
    if (input$q3 == "车重每增加1单位，油耗平均减少5.344单位") {
      "正确！根据模型输出，wt的系数为-5.344，表示车重每增加1000磅，油耗平均减少5.344英里/加仑。"
    } else {
      "不正确。请仔细查看系数表，wt的系数为-5.344，表示车重每增加1000磅，油耗平均减少5.344英里/加仑。"
    }
  })
  
  # 练习4
  output$answer_q4 <- renderText({
    req(input$check_q4)
    correct_answer <- 10 + 2.5 * 5  # = 22.5
    
    user_answer <- as.numeric(input$q4)
    
    if (is.na(user_answer)) {
      "请输入一个数值。"
    } else if (abs(user_answer - correct_answer) < 0.01) {
      "正确！y = 10 + 2.5 × 5 = 22.5，预测销售额为22.5万元。"
    } else {
      paste0("不正确。正确答案是22.5万元。计算过程: y = 10 + 2.5 × 5 = 22.5")
    }
  })
}

# 运行应用程序
shinyApp(ui = ui, server = server) 