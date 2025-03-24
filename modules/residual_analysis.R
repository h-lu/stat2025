library(shiny)
library(ggplot2)
library(dplyr)
library(car) # 用于residualPlots

# 残差分析模块
# 演示不同类型的残差模式及其诊断方法

# 定义示例数据
# 定义样本数据和模型
sample_data_list <- list(
  "正常" = data.frame(
    x = seq(0, 10, length.out = 30),
    y = 2 + 1.5 * seq(0, 10, length.out = 30) + rnorm(30, 0, 1)
  ),
  "异方差" = data.frame(
    x = seq(0, 10, length.out = 30),
    y = 2 + 1.5 * seq(0, 10, length.out = 30) + rnorm(30, 0, seq(0.2, 2, length.out = 30))
  ),
  "非线性" = data.frame(
    x = seq(0, 10, length.out = 30),
    y = 2 + 1.5 * seq(0, 10, length.out = 30) + 0.3 * seq(0, 10, length.out = 30)^2 + rnorm(30, 0, 1)
  ),
  "离群值" = {
    df <- data.frame(
      x = seq(0, 10, length.out = 30),
      y = 2 + 1.5 * seq(0, 10, length.out = 30) + rnorm(30, 0, 1)
    )
    df$y[c(5, 15, 25)] <- df$y[c(5, 15, 25)] + c(5, -6, 7)
    df
  },
  "自相关" = data.frame(
    x = seq(0, 10, length.out = 30),
    y = 2 + 1.5 * seq(0, 10, length.out = 30) + c(0, filter(rnorm(30, 0, 0.8), 0.8, method = "recursive")[-30])
  )
)

sample_models <- lapply(sample_data_list, function(data) {
  lm(y ~ x, data = data)
})

residualAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             h3("残差分析工具"),
             p("本工具演示如何分析回归模型的残差，用于评估模型假设和拟合质量。")
      )
    ),
    fluidRow(
      column(3,
             wellPanel(
               h4("数据设置"),
               selectInput(ns("pattern"), "残差模式:", 
                          choices = c("正常" = "正常", 
                                     "异方差" = "异方差", 
                                     "非线性" = "非线性",
                                     "自相关" = "自相关",
                                     "离群值" = "离群值")),
               
               sliderInput(ns("noise"), "噪声水平:", 
                          min = 0.1, max = 1, value = 0.5, step = 0.1),
               
               actionButton(ns("generate"), "生成新数据", 
                           class = "btn-success"),
               
               hr(),
               
               h4("诊断图类型"),
               radioButtons(ns("plot_type"), "选择诊断图:", 
                           choices = c("残差 vs 预测值" = "fitted",
                                      "残差 vs 自变量" = "predictor",
                                      "残差直方图" = "histogram",
                                      "残差Q-Q图" = "qq",
                                      "标准化残差" = "standardized",
                                      "杠杆值" = "leverage")),
               
               hr(),
               
               h4("残差模式解释"),
               htmlOutput(ns("pattern_explanation"))
             )
      ),
      column(9,
             tabsetPanel(
               tabPanel("诊断图", 
                       plotOutput(ns("diagnostic_plot"), height = "400px")),
               tabPanel("数据图", 
                       plotOutput(ns("data_plot"), height = "400px")),
               tabPanel("诊断结果", 
                       verbatimTextOutput(ns("diagnostics_results")))
             )
      )
    )
  )
}

residualAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 反应式数据集
    df <- reactiveVal(NULL)
    
    # 生成新数据
    observeEvent(input$generate, {
      pattern <- input$pattern
      noise_level <- input$noise
      
      new_data <- generate_pattern_data(pattern, n = 100, noise = noise_level)
      df(new_data)
    })
    
    # 初始化时自动生成数据
    observe({
      if(is.null(df())) {
        # 触发生成按钮
        session$sendCustomMessage(type = 'click', message = list(id = session$ns('generate')))
      }
    })
    
    # 拟合线性模型
    model <- reactive({
      data <- df()
      
      # 确保数据框不为空且包含x和y列
      if(is.null(data) || nrow(data) == 0 || 
         !"x" %in% names(data) || !"y" %in% names(data)) {
        return(NULL)
      }
      
      tryCatch({
        lm(y ~ x, data = data)
      }, error = function(e) {
        NULL
      })
    })
    
    # 渲染数据图
    output$data_plot <- renderPlot({
      data <- df()
      
      if(is.null(data) || nrow(data) == 0) {
        return(ggplot() + 
              labs(title = "无可用数据", 
                  subtitle = "请点击'生成新数据'按钮") + 
              theme_minimal(base_size = 14))
      }
      
      # 获取基础模型
      m <- model()
      
      if(is.null(m)) {
        return(ggplot() + 
              labs(title = "无可用模型", 
                  subtitle = "请检查数据生成") + 
              theme_minimal(base_size = 14))
      }
      
      # 添加预测值
      data$fitted <- predict(m)
      
      ggplot(data, aes(x = x, y = y)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", formula = y ~ x, 
                   color = "blue", linewidth = 1, se = TRUE) +
        labs(
          title = paste0("数据模式: ", input$pattern),
          subtitle = paste0("噪声水平: ", input$noise),
          x = "X变量",
          y = "Y变量"
        ) +
        theme_minimal(base_size = 14)
    })
    
    # 渲染诊断图
    output$diagnostic_plot <- renderPlot({
      data <- df()
      m <- model()
      
      if(is.null(data) || nrow(data) == 0) {
        return(ggplot() + 
              labs(title = "无可用数据", 
                  subtitle = "请点击'生成新数据'按钮") + 
              theme_minimal(base_size = 14))
      }
      
      if(is.null(m)) {
        return(ggplot() + 
              labs(title = "无可用模型", 
                  subtitle = "请检查数据生成") + 
              theme_minimal(base_size = 14))
      }
      
      # 选择诊断图类型
      plot_type <- input$plot_type
      
      # 提取诊断数据
      fitted_vals <- fitted(m)
      residuals_vals <- residuals(m)
      std_resid <- rstandard(m)
      leverage <- hatvalues(m)
      
      # 根据选择生成诊断图
      if(plot_type == "fitted") {
        # 残差 vs 预测值
        diag_data <- data.frame(fitted = fitted_vals, residuals = residuals_vals)
        
        p <- ggplot(diag_data, aes(x = fitted, y = residuals)) +
          geom_point(size = 3, alpha = 0.7) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          labs(
            title = "残差 vs 预测值",
            subtitle = "理想情况：随机分布在零线周围，无明显模式",
            x = "预测值",
            y = "残差"
          )
      } else if(plot_type == "predictor") {
        # 残差 vs 自变量
        diag_data <- data.frame(x = data$x, residuals = residuals_vals)
        
        p <- ggplot(diag_data, aes(x = x, y = residuals)) +
          geom_point(size = 3, alpha = 0.7) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          labs(
            title = "残差 vs 自变量",
            subtitle = "理想情况：随机分布在零线周围，无明显模式",
            x = "X变量",
            y = "残差"
          )
      } else if(plot_type == "histogram") {
        # 残差直方图
        diag_data <- data.frame(residuals = residuals_vals)
        
        p <- ggplot(diag_data, aes(x = residuals)) +
          geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
          labs(
            title = "残差直方图",
            subtitle = "理想情况：残差呈正态分布",
            x = "残差",
            y = "频数"
          )
      } else if(plot_type == "qq") {
        # 残差Q-Q图
        diag_data <- data.frame(std_resid = std_resid)
        
        p <- ggplot(diag_data, aes(sample = std_resid)) +
          stat_qq() +
          stat_qq_line(color = "red") +
          labs(
            title = "残差Q-Q图",
            subtitle = "理想情况：点落在对角线上，表示残差呈正态分布",
            x = "理论分位数",
            y = "样本分位数"
          )
      } else if(plot_type == "standardized") {
        # 标准化残差 vs 预测值
        diag_data <- data.frame(fitted = fitted_vals, std_resid = std_resid)
        
        p <- ggplot(diag_data, aes(x = fitted, y = std_resid)) +
          geom_point(size = 3, alpha = 0.7) +
          geom_hline(yintercept = c(-2, 0, 2), linetype = c("dashed", "solid", "dashed"), 
                    color = c("red", "black", "red")) +
          labs(
            title = "标准化残差 vs 预测值",
            subtitle = "理想情况：95%的点应在±2范围内，随机分布",
            x = "预测值",
            y = "标准化残差"
          )
      } else if(plot_type == "leverage") {
        # 杠杆值与标准化残差
        cook_d <- cooks.distance(m)
        diag_data <- data.frame(leverage = leverage, std_resid = std_resid, cook_d = cook_d)
        
        p <- ggplot(diag_data, aes(x = leverage, y = std_resid)) +
          geom_point(aes(size = cook_d), alpha = 0.7) +
          geom_hline(yintercept = 0, linetype = "solid", color = "black") +
          geom_vline(xintercept = 2 * mean(leverage), linetype = "dashed", color = "red") +
          labs(
            title = "杠杆值 vs 标准化残差 (Cook距离)",
            subtitle = "点大小代表Cook距离。右上/右下区域的大点表示高影响点",
            x = "杠杆值",
            y = "标准化残差",
            size = "Cook距离"
          ) +
          scale_size_continuous(range = c(1, 10))
      }
      
      p + theme_minimal(base_size = 14)
    })
    
    # 渲染诊断结果
    output$diagnostics_results <- renderText({
      m <- model()
      
      if(is.null(m)) {
        return("无可用模型。请生成数据并检查模型拟合。")
      }
      
      # 总结诊断结果
      pattern <- input$pattern
      
      # 模型摘要
      sum_m <- summary(m)
      r_squared <- round(sum_m$r.squared, 4)
      adj_r_squared <- round(sum_m$adj.r.squared, 4)
      
      # 评估异方差性
      bp_test <- try(car::ncvTest(m), silent = TRUE)
      if(inherits(bp_test, "try-error")) {
        bp_result <- "无法执行异方差性检验"
      } else {
        bp_p <- round(bp_test$p, 4)
        bp_result <- ifelse(bp_p < 0.05, 
                          paste0("存在异方差性 (p = ", bp_p, ")"), 
                          paste0("无异方差性 (p = ", bp_p, ")"))
      }
      
      # 评估自相关性
      dw_test <- try(car::durbinWatsonTest(m), silent = TRUE)
      if(inherits(dw_test, "try-error")) {
        dw_result <- "无法执行自相关性检验"
      } else {
        dw_p <- round(dw_test$p, 4)
        dw_result <- ifelse(dw_p < 0.05, 
                         paste0("存在自相关性 (p = ", dw_p, ")"), 
                         paste0("无自相关性 (p = ", dw_p, ")"))
      }
      
      # 评估残差正态性
      sw_test <- try(shapiro.test(residuals(m)), silent = TRUE)
      if(inherits(sw_test, "try-error")) {
        sw_result <- "无法执行正态性检验"
      } else {
        sw_p <- round(sw_test$p.value, 4)
        sw_result <- ifelse(sw_p < 0.05, 
                         paste0("残差不服从正态分布 (p = ", sw_p, ")"), 
                         paste0("残差服从正态分布 (p = ", sw_p, ")"))
      }
      
      # 检测异常值
      outliers <- which(abs(rstandard(m)) > 2)
      n_outliers <- length(outliers)
      outlier_result <- ifelse(n_outliers > 0, 
                            paste0("检测到", n_outliers, "个离群值 (标准化残差>2)"), 
                            "未检测到离群值")
      
      # 检测高影响点
      cook_d <- cooks.distance(m)
      influential <- which(cook_d > 4/(length(cook_d)-2))
      n_influential <- length(influential)
      influential_result <- ifelse(n_influential > 0, 
                                paste0("检测到", n_influential, "个高影响点 (Cook距离较大)"), 
                                "未检测到高影响点")
      
      # 整合结果
      paste0(
        "回归诊断结果摘要:\n\n",
        "模型拟合度:\n",
        "  R² = ", r_squared, "\n",
        "  调整R² = ", adj_r_squared, "\n\n",
        "残差诊断:\n",
        "  异方差性: ", bp_result, "\n",
        "  自相关性: ", dw_result, "\n",
        "  正态性: ", sw_result, "\n",
        "  离群值: ", outlier_result, "\n",
        "  高影响点: ", influential_result, "\n\n",
        "数据模式: ", pattern, "\n",
        "噪声水平: ", input$noise, "\n\n",
        "解释: ", get_pattern_interpretation(pattern)
      )
    })
    
    # 渲染模式解释
    output$pattern_explanation <- renderUI({
      explanation <- get_pattern_explanation(input$pattern)
      HTML(explanation)
    })
    
    # 获取模式解释
    get_pattern_explanation <- function(pattern) {
      explanations <- list(
        "正常" = "残差呈随机分布，没有特定模式，符合线性回归假设。诊断图显示残差在零线周围均匀分布，没有系统性模式。",
        
        "异方差" = "残差方差不恒定，通常表现为漏斗形。这违反了线性回归的同方差假设，可能导致标准误估计不准确。<br><br>可能原因：
        <ul>
          <li>随机误差与预测变量或响应变量相关</li>
          <li>数据尺度问题（如对数转换可能有帮助）</li>
          <li>模型缺少重要交互项</li>
        </ul>",
        
        "非线性" = "残差图中出现明显的曲线模式，表明线性模型不足以捕捉变量之间的关系。<br><br>可能原因：
        <ul>
          <li>真实关系是非线性的（如二次、对数或指数）</li>
          <li>模型缺少重要的高阶项</li>
          <li>需要对变量进行变换</li>
        </ul>",
        
        "自相关" = "残差之间存在相关性，通常在时间序列或空间数据中出现。表现为残差图中的系统性模式（如波浪形）。<br><br>可能原因：
        <ul>
          <li>数据有时间或空间依赖性</li>
          <li>模型缺少重要的滞后变量</li>
          <li>需要使用考虑自相关的专门模型</li>
        </ul>",
        
        "离群值" = "数据中存在极端值，它们在残差图中表现为远离主体的点。这些点可能对模型估计产生过大影响。<br><br>可能原因：
        <ul>
          <li>数据收集或记录错误</li>
          <li>特殊情况或罕见事件</li>
          <li>模型未能捕捉某些子群体的行为</li>
        </ul>"
      )
      
      return(explanations[[pattern]])
    }
    
    # 获取模式解释文本
    get_pattern_interpretation <- function(pattern) {
      interpretations <- list(
        "正常" = "数据符合线性回归假设，残差随机分布，无明显模式。",
        "异方差" = "数据存在异方差性，残差方差不恒定，可能需要方差稳定化变换。",
        "非线性" = "数据存在非线性关系，线性模型不足以捕捉变量间关系，考虑添加高阶项或变换。",
        "自相关" = "数据存在自相关性，残差之间相互关联，考虑时间序列模型或添加滞后变量。",
        "离群值" = "数据中存在异常值，它们可能对模型估计产生不当影响，考虑鲁棒回归方法。"
      )
      
      return(interpretations[[pattern]])
    }
  })
} 