# RSS计算器模块
# 演示残差平方和(RSS)的计算和最小化

rssCalculatorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("残差平方和(RSS)计算器"),
        p("调整参数，观察残差平方和如何变化。RSS是实际值和预测值之间差异的平方和，是衡量模型拟合优度的关键指标。")
      )
    ),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          sliderInput(ns("intercept"), "截距 (β₀):", 
                     min = -10, max = 20, value = 5, step = 0.5),
          sliderInput(ns("slope"), "斜率 (β₁):", 
                     min = -5, max = 10, value = 2, step = 0.5),
          selectInput(ns("points"), "显示数据点:", 
                     choices = c("全部", "选定的点"), selected = "全部"),
          checkboxInput(ns("show_residuals"), "显示残差", TRUE),
          checkboxInput(ns("show_squares"), "显示残差平方", FALSE),
          actionButton(ns("find_optimal"), "找到最优参数", class = "btn-primary btn-block"),
          hr(),
          h4("残差平方和 (RSS)"),
          textOutput(ns("rss_value")),
          h4("决定系数 R²"),
          textOutput(ns("r_squared"))
        )
      ),
      column(
        width = 9,
        plotOutput(ns("rss_plot"), height = "400px"),
        br(),
        h4("残差表"),
        DT::dataTableOutput(ns("residuals_table"))
      )
    )
  )
}

rssCalculatorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 生成随机数据
    data <- reactiveVal(generate_linear_data(n = 15))
    
    # 计算最优参数
    optimal_params <- reactive({
      calculate_ols_params(data()$x, data()$y)
    })
    
    # 计算当前RSS和R²
    current_metrics <- reactive({
      d <- data()
      intercept <- input$intercept
      slope <- input$slope
      
      # 计算预测值和残差
      d$predicted <- intercept + slope * d$x
      d$residuals <- d$y - d$predicted
      d$residuals_squared <- d$residuals^2
      
      # 计算RSS和R²
      rss <- sum(d$residuals_squared)
      r_squared <- calculate_r_squared(d$x, d$y, intercept, slope)
      
      list(
        data = d,
        rss = rss,
        r_squared = r_squared
      )
    })
    
    # 当点击"找到最优参数"按钮时更新参数
    observeEvent(input$find_optimal, {
      opt <- optimal_params()
      updateSliderInput(session, "intercept", value = opt$intercept)
      updateSliderInput(session, "slope", value = opt$slope)
    })
    
    # 显示RSS值
    output$rss_value <- renderText({
      metrics <- current_metrics()
      opt_rss <- calculate_rss(
        data()$x, data()$y, 
        optimal_params()$intercept, optimal_params()$slope
      )
      
      if(near(metrics$rss, opt_rss, tol = 0.1)) {
        sprintf("%.2f (最小值!)", metrics$rss)
      } else {
        sprintf("%.2f (最小可能值: %.2f)", metrics$rss, opt_rss)
      }
    })
    
    # 显示R²值
    output$r_squared <- renderText({
      metrics <- current_metrics()
      sprintf("%.4f", metrics$r_squared)
    })
    
    # 绘制RSS图
    output$rss_plot <- renderPlot({
      metrics <- current_metrics()
      d <- metrics$data
      
      # 检查输入值是否为NA
      if(is.null(input$intercept) || is.null(input$slope) || 
         is.na(input$intercept) || is.na(input$slope)) {
        return(ggplot(d, aes(x = x, y = y)) +
               geom_point(size = 3, alpha = 0.7) +
               labs(title = "请调整滑块设置参数值", 
                    subtitle = "使用左侧的滑块设置截距和斜率") +
               theme_minimal(base_size = 14))
      }
      
      intercept <- input$intercept
      slope <- input$slope
      
      # 预测值和残差
      d$predicted <- intercept + slope * d$x
      d$residuals <- d$y - d$predicted
      
      # 计算当前RSS值
      current_rss <- sum(d$residuals^2)
      
      # 计算最佳参数
      best <- optimal_params()
      
      # 最佳拟合线
      best_intercept <- best$intercept
      best_slope <- best$slope
      
      # 绘制散点图和拟合线
      p <- ggplot(d, aes(x = x, y = y)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_abline(intercept = intercept, slope = slope, 
                   color = "blue", linewidth = 1.2) +
        geom_segment(aes(xend = x, yend = predicted), 
                    color = "red", alpha = 0.5) +
        labs(title = paste("残差平方和 (RSS) =", round(current_rss, 2)),
             subtitle = "红线表示残差 (实际值 - 预测值)") +
        theme_minimal(base_size = 14)
      
      # 如果显示最佳拟合，则添加最佳拟合线
      if(input$show_residuals && !is.null(best) && 
         !is.na(best_intercept) && !is.na(best_slope)) {
        best_rss <- calculate_rss(d$x, d$y, best_intercept, best_slope)
        p <- p + 
          geom_abline(intercept = best_intercept, slope = best_slope, 
                     color = "green", linetype = "dashed", linewidth = 1.2) +
          labs(subtitle = paste0("红线: 当前残差, 绿线: 最佳拟合 (最小RSS = ", 
                                round(best_rss, 2), ")"))
      }
      
      p
    })
    
    # 显示残差表
    output$residuals_table <- DT::renderDataTable({
      metrics <- current_metrics()
      d <- metrics$data
      
      df_display <- data.frame(
        "点" = 1:nrow(d),
        "X值" = round(d$x, 2),
        "Y值" = round(d$y, 2),
        "预测值" = round(d$predicted, 2),
        "残差" = round(d$residuals, 2),
        "残差平方" = round(d$residuals_squared, 2)
      )
      
      DT::datatable(
        df_display,
        options = list(
          pageLength = 5,
          lengthChange = FALSE,
          searching = FALSE
        )
      )
    })
  })
} 