library(shiny)
library(ggplot2)
library(dplyr)

# 回归线调整器模块
# 允许用户直观地调整回归线并观察结果

regressionLineAdjusterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("回归线参数调整器"),
        p("点击并拖动图表上的蓝点来直观地调整回归线。观察不同参数如何影响预测值和拟合优度。")
      )
    ),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("模型参数"),
          htmlOutput(ns("current_params")),
          hr(),
          h4("拟合统计"),
          p("残差平方和 (RSS):"),
          textOutput(ns("adj_rss")),
          p("决定系数 (R²):"),
          textOutput(ns("adj_r_squared")),
          hr(),
          actionButton(ns("reset_adj"), "重置到最佳拟合", class = "btn-primary btn-block"),
          actionButton(ns("new_data"), "生成新数据", class = "btn-secondary btn-block")
        )
      ),
      column(
        width = 9,
        plotOutput(ns("regression_plot"), height = "400px", click = ns("plot_click"), hover = ns("plot_hover")),
        p("使用方法: 点击并拖动调整蓝色截距点和橙色斜率点。截距点的位置决定了回归线与Y轴的交点，而橙色点与蓝色点的连线决定了斜率。"),
        checkboxInput(ns("show_drag_hints"), "显示拖拽提示", TRUE)
      )
    )
  )
}

regressionLineAdjusterServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 初始数据
    adj_data <- reactiveVal(generate_linear_data(n = 30))
    
    # 初始最优参数
    optimal_fit <- reactive({
      calculate_ols_params(adj_data()$x, adj_data()$y)
    })
    
    # 当前参数
    current_params <- reactiveVal(NULL)
    
    # 初始化参数
    observe({
      if (is.null(current_params())) {
        opt <- optimal_fit()
        current_params(list(
          intercept = opt$intercept,
          slope = opt$slope,
          slope_point_x = 8,  # 固定斜率调整点的x坐标
          slope_point_y = opt$intercept + opt$slope * 8  # 计算对应的y坐标
        ))
      }
    })
    
    # 更新当前参数
    observeEvent(input$plot_click, {
      # 获取点击位置
      click_x <- input$plot_click$x
      click_y <- input$plot_click$y
      
      params <- current_params()
      
      # 计算点击位置到可拖动点的距离
      dist_to_intercept <- sqrt((click_x)^2 + (click_y - params$intercept)^2)
      dist_to_slope_point <- sqrt((click_x - params$slope_point_x)^2 + 
                                  (click_y - params$slope_point_y)^2)
      
      # 拖动阈值 - 决定多远距离内被视为点击到点
      drag_threshold <- 0.8
      
      # 如果点击接近截距点
      if (dist_to_intercept < drag_threshold) {
        # 更新截距，保持斜率不变
        new_intercept <- click_y
        new_slope <- params$slope
        params$slope_point_y <- new_intercept + new_slope * params$slope_point_x
        params$intercept <- new_intercept
      } 
      # 如果点击接近斜率点
      else if (dist_to_slope_point < drag_threshold) {
        # 更新斜率点位置，重新计算斜率
        params$slope_point_y <- click_y
        params$slope <- (params$slope_point_y - params$intercept) / params$slope_point_x
      }
      
      current_params(params)
    })
    
    # 重置参数按钮
    observeEvent(input$reset_adj, {
      opt <- optimal_fit()
      current_params(list(
        intercept = opt$intercept,
        slope = opt$slope,
        slope_point_x = 8,
        slope_point_y = opt$intercept + opt$slope * 8
      ))
    })
    
    # 生成新数据按钮
    observeEvent(input$new_data, {
      new_data <- generate_linear_data(n = 30)
      adj_data(new_data)
      
      # 重置参数到最佳拟合
      opt <- calculate_ols_params(new_data$x, new_data$y)
      current_params(list(
        intercept = opt$intercept,
        slope = opt$slope,
        slope_point_x = 8,
        slope_point_y = opt$intercept + opt$slope * 8
      ))
    })
    
    # 计算当前RSS和R²
    current_fit_stats <- reactive({
      params <- current_params()
      data <- adj_data()
      
      if (is.null(params)) return(list(rss = NA, r_squared = NA))
      
      rss <- calculate_rss(data$x, data$y, params$intercept, params$slope)
      r_squared <- calculate_r_squared(data$x, data$y, params$intercept, params$slope)
      
      list(rss = rss, r_squared = r_squared)
    })
    
    # 显示当前参数
    output$current_params <- renderUI({
      params <- current_params()
      if (is.null(params)) return(NULL)
      
      HTML(sprintf(
        "<strong>intercept (β₀):</strong> %.2f<br>
         <strong>slope (β₁):</strong> %.2f<br>
         <strong>equation:</strong> Y = %.2f + %.2f · X",
        params$intercept, params$slope, params$intercept, params$slope
      ))
    })
    
    # 显示RSS
    output$adj_rss <- renderText({
      stats <- current_fit_stats()
      sprintf("%.2f", stats$rss)
    })
    
    # 显示R²
    output$adj_r_squared <- renderText({
      stats <- current_fit_stats()
      sprintf("%.4f", stats$r_squared)
    })
    
    # 绘制调整图
    output$regression_plot <- renderPlot({
      params <- current_params()
      data <- adj_data()
      
      if (is.null(params)) return(NULL)
      
      # 计算预测值和残差
      predicted <- params$intercept + params$slope * data$x
      residuals <- data$y - predicted
      
      # 基础散点图
      p <- ggplot(data, aes(x = x, y = y)) +
        geom_point(size = 3, alpha = 0.8) +
        geom_abline(
          intercept = params$intercept, 
          slope = params$slope, 
          color = "blue", 
          linewidth = 1.2
        ) +
        geom_segment(
          aes(x = x, y = y, xend = x, yend = predicted), 
          color = "red", 
          alpha = 0.4
        ) +
        labs(
          title = "拖拽调整回归线", 
          subtitle = sprintf(
            "equation: y = %.2f + %.2f · x    RSS: %.2f    R²: %.4f", 
            params$intercept, params$slope, 
            current_fit_stats()$rss, current_fit_stats()$r_squared
          )
        ) +
        theme_minimal(base_size = 14)
      
      # 添加可拖动点
      drag_points <- data.frame(
        x = c(0, params$slope_point_x),
        y = c(params$intercept, params$slope_point_y),
        type = c("intercept", "slope")
      )
      
      p <- p + geom_point(
        data = drag_points,
        aes(x = x, y = y, color = type),
        size = 5,
        alpha = 0.7
      ) +
        scale_color_manual(values = c("intercept" = "blue", "slope" = "orange"))
      
      # 如果选择显示拖拽提示
      if (input$show_drag_hints) {
        p <- p + 
          geom_path(
            data = drag_points,
            aes(x = x, y = y),
            color = "gray50",
            linetype = "dashed"
          ) +
          annotate(
            "text", 
            x = 0.5, 
            y = params$intercept + 0.8, 
            label = "拖动调整截距",
            color = "blue",
            hjust = 0
          ) +
          annotate(
            "text", 
            x = params$slope_point_x - 0.5, 
            y = params$slope_point_y + 0.8, 
            label = "拖动调整斜率",
            color = "orange", 
            hjust = 1
          )
      }
      
      # 标记是否为最优拟合
      opt <- optimal_fit()
      is_optimal <- near(params$intercept, opt$intercept, tol = 0.1) && 
                   near(params$slope, opt$slope, tol = 0.1)
      
      if (is_optimal) {
        p <- p + labs(caption = "当前状态: 最佳拟合!")
      }
      
      p + theme(legend.position = "none")
    })
    
    # 解释系数
    output$coef_interpretation <- renderUI({
      params <- current_params()
      if (is.null(params)) return(NULL)

      if (is.na(params$slope)) return(NULL)  # 确保斜率不是NA
      
      intercept_interp <- sprintf(
        "intercept (β₀ = %.2f): when x = 0, the predicted y value.", 
        params$intercept
      )
      
      slope_interp <- sprintf(
        "slope (β₁ = %.2f): x increases by 1 unit, y increases by average %.2f units.", 
        params$slope, params$slope
      )
      
      direction <- if (params$slope > 0) "positive" else "negative"
      
      relationship <- sprintf(
        "This indicates a %s relationship between x and y.", 
        direction
      )
      
      HTML(paste(
        intercept_interp, slope_interp, relationship,
        sep = "<br><br>"
      ))
    })
  })
} 