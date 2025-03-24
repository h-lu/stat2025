# 简化版OLS演示模块
# 演示最小二乘法的工作原理

library(shiny)
library(ggplot2)

olsDemoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("最小二乘法演示"),
        p("使用下面的滑块调整截距和斜率，观察拟合线如何变化。
           最小二乘法寻找使残差平方和(RSS)最小的参数。")
      )
    ),
    fluidRow(
      column(
        width = 4,
        sliderInput(ns("ols_intercept"), "截距 (β₀):", 
                    min = -10, max = 20, value = 5, step = 0.5),
        sliderInput(ns("ols_slope"), "斜率 (β₁):", 
                    min = -3, max = 7, value = 2, step = 0.1),
        actionButton(ns("reset_params"), "重置参数", class = "btn-secondary"),
        actionButton(ns("find_best"), "寻找最佳拟合", class = "btn-primary"),
        br(), br(),
        wellPanel(
          h4("回归方程"),
          htmlOutput(ns("equation")),
          h4("残差平方和 (RSS)"),
          uiOutput(ns("rss_value"))
        )
      ),
      column(
        width = 8,
        plotOutput(ns("regression_plot"), height = "400px"),
        
        hr(),
        
        fluidRow(
          column(6,
            h4("实时更新"),
            checkboxInput(ns("show_lines"), "显示到回归线的距离", TRUE),
            checkboxInput(ns("show_squares"), "显示残差平方", TRUE)
          ),
          column(6,
            conditionalPanel(
              condition = "input.find_best", 
              ns = ns,
              h4("模型表现"),
              verbatimTextOutput(ns("model_performance"))
            )
          )
        )
      )
    )
  )
}

olsDemoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 生成散点图数据(x, y)
    points <- reactive({
      # 固定的种子以确保一致性
      set.seed(123)
      n <- 20  # 样本数量
      
      x <- runif(n, 1, 10)
      y <- 5 + 2 * x + rnorm(n, 0, 3)  # 真实模型: y = 5 + 2x + 噪声
      
      # 记住样本均值，用于计算R²
      mean_y <- mean(y)
      
      data.frame(x = x, y = y, mean_y = mean_y)
    })
    
    # 重置参数按钮
    observeEvent(input$reset_params, {
      updateSliderInput(session, "ols_intercept", value = 5)
      updateSliderInput(session, "ols_slope", value = 2)
    })
    
    # 计算当前参数下的RSS
    calculate_rss <- reactive({
      req(points(), !is.null(input$ols_intercept), !is.null(input$ols_slope))
      
      # 防止NA值
      if(is.na(input$ols_intercept) || is.na(input$ols_slope)) {
        return(NA)
      }
      
      intercept <- input$ols_intercept
      slope <- input$ols_slope
      
      df <- points()
      predicted <- intercept + slope * df$x
      residuals <- df$y - predicted
      sum(residuals^2)
    })
    
    # 计算最佳拟合参数
    best_fit <- reactive({
      df <- points()
      model <- lm(y ~ x, data = df)
      
      list(
        intercept = coef(model)[1],
        slope = coef(model)[2],
        rss = sum(residuals(model)^2),
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        model = model
      )
    })
    
    # 检查当前参数是否接近最佳拟合
    is_best_fit <- reactive({
      req(!is.null(input$ols_intercept), !is.null(input$ols_slope))
      
      # 防止NA值
      if(is.na(input$ols_intercept) || is.na(input$ols_slope)) {
        return(FALSE)
      }
      
      best <- best_fit()
      
      # 确保best不为NULL，且其组件不为NA
      if(is.null(best) || is.na(best$intercept) || is.na(best$slope)) {
        return(FALSE)
      }
      
      # 判断是否接近最佳参数(允许小误差)
      intercept_diff <- abs(input$ols_intercept - best$intercept)
      slope_diff <- abs(input$ols_slope - best$slope)
      
      intercept_diff < 0.5 && slope_diff < 0.2
    })
    
    # 寻找最佳拟合按钮
    observeEvent(input$find_best, {
      best <- best_fit()
      updateSliderInput(session, "ols_intercept", value = round(best$intercept, 1))
      updateSliderInput(session, "ols_slope", value = round(best$slope, 1))
    })
    
    # 渲染回归方程
    output$equation <- renderUI({
      req(!is.null(input$ols_intercept), !is.null(input$ols_slope))
      
      # 防止NA值
      if(is.na(input$ols_intercept) || is.na(input$ols_slope)) {
        return(HTML("<p>请调整参数滑块以获取回归方程。</p>"))
      }
      
      intercept <- round(input$ols_intercept, 2)
      slope <- round(input$ols_slope, 2)
      
      # 构建方程字符串
      if(slope >= 0) {
        eq <- paste0("y = ", intercept, " + ", slope, "x")
      } else {
        eq <- paste0("y = ", intercept, " - ", abs(slope), "x")
      }
      
      # 额外显示最佳拟合指示
      if(is_best_fit()) {
        eq <- paste0(eq, " <span style='color: green;'>(最佳拟合)</span>")
      }
      
      HTML(paste0("<p style='font-size: 16px;'>", eq, "</p>"))
    })
    
    # 渲染RSS值
    output$rss_value <- renderUI({
      req(!is.null(calculate_rss()))
      
      # 防止NA值
      if(is.na(calculate_rss())) {
        return(HTML("<p>请调整参数滑块以计算RSS。</p>"))
      }
      
      rss <- round(calculate_rss(), 2)
      best <- best_fit()
      min_rss <- round(best$rss, 2)
      
      if(rss == min_rss) {
        HTML(paste0("<p style='font-size: 16px;'>RSS = ", rss, 
                  " <span style='color: green;'>(最小值)</span></p>"))
      } else {
        diff_from_min <- round(rss - min_rss, 2)
        HTML(paste0("<p style='font-size: 16px;'>RSS = ", rss, 
                  " <span style='color: orange;'>(比最小值大 ", diff_from_min, ")</span></p>"))
      }
    })
    
    # 渲染模型表现
    output$model_performance <- renderText({
      best <- best_fit()
      
      paste0(
        "模型表现指标:\n\n",
        "R² (决定系数): ", round(best$r_squared, 4), "\n",
        "调整后R²: ", round(best$adj_r_squared, 4), "\n",
        "RSS (残差平方和): ", round(best$rss, 4), "\n",
        "RMSE (均方根误差): ", round(sqrt(best$rss / length(points()$x)), 4)
      )
    })
    
    # 渲染OLS图
    output$regression_plot <- renderPlot({
      req(points(), !is.null(input$ols_intercept), !is.null(input$ols_slope))
      
      # 防止NA值
      if(is.na(input$ols_intercept) || is.na(input$ols_slope)) {
        return(ggplot() + 
               labs(title = "请调整参数滑块",
                    subtitle = "当前参数包含NA值") + 
               theme_minimal(base_size = 14))
      }
      
      df <- points()
      intercept <- input$ols_intercept
      slope <- input$ols_slope
      
      # 计算预测值和残差
      df$predicted <- intercept + slope * df$x
      df$residuals <- df$y - df$predicted
      
      # 创建基础散点图
      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = "最小二乘法原理演示",
          subtitle = paste0("当前参数: 截距 = ", round(intercept, 2), 
                          ", 斜率 = ", round(slope, 2), 
                          ", RSS = ", round(calculate_rss(), 2)),
          x = "X变量",
          y = "Y变量"
        ) +
        theme_minimal(base_size = 14)
      
      # 添加回归线
      p <- p + geom_abline(
        intercept = intercept, 
        slope = slope, 
        color = "blue", 
        linewidth = 1.2
      )
      
      # 可选显示到回归线的距离(残差)
      if(input$show_lines) {
        p <- p + geom_segment(
          aes(xend = x, yend = predicted), 
          color = "red", 
          alpha = 0.7, 
          linewidth = 0.7
        )
      }
      
      # 可选显示残差平方
      if(input$show_squares) {
        # 创建代表残差平方的正方形
        squares <- data.frame()
        for(i in 1:nrow(df)) {
          x_val <- df$x[i]
          y_val <- df$y[i]
          pred_val <- df$predicted[i]
          res <- y_val - pred_val
          
          # 只为残差较大的点创建方框(为了可视化效果)
          if(abs(res) > 0.5) {
            # 方框的四个角
            x_min <- min(x_val, x_val)
            x_max <- x_min + abs(res)
            y_min <- min(pred_val, y_val)
            y_max <- max(pred_val, y_val)
            
            # 添加到数据框
            square <- data.frame(
              x = c(x_min, x_max, x_max, x_min, x_min),
              y = c(y_min, y_min, y_max, y_max, y_min),
              residual = res,
              group = i
            )
            squares <- rbind(squares, square)
          }
        }
        
        if(nrow(squares) > 0) {
          p <- p + geom_polygon(
            data = squares, 
            aes(group = group), 
            fill = "red", 
            alpha = 0.2
          )
        }
      }
      
      # 如果发现最佳拟合，添加标签
      if(is_best_fit()) {
        p <- p + annotate(
          "text",
          x = max(df$x),
          y = min(df$y),
          label = "当前是最佳拟合!",
          hjust = 1,
          vjust = 0,
          color = "darkgreen",
          size = 5
        )
      }
      
      p
    })
  })
} 