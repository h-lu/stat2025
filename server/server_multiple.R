# server_multiple.R - 多元回归模块的服务器逻辑

# 定义多元回归模块的服务器逻辑
server_multiple <- function(input, output, session) {
  # 多元回归模型
  multi_model <- reactive({
    req(input$multi_x_vars)
    
    # 构建公式
    formula <- as.formula(paste("medv ~", paste(input$multi_x_vars, collapse = " + ")))
    
    # 拟合模型
    model <- lm(formula, data = Boston)
    
    return(model)
  })
  
  # 显示模型摘要
  output$multi_summary <- renderPrint({
    req(multi_model())
    summary(multi_model())
  })
  
  # 系数图
  output$multi_coef_plot <- renderPlotly({
    req(multi_model())
    model <- multi_model()
    
    # 提取系数（不包括截距）
    coefs <- coef(model)[-1]
    
    # 创建数据框
    df <- data.frame(
      Variable = names(coefs),
      Coefficient = coefs
    )
    
    # 按系数绝对值排序
    df <- df[order(abs(df$Coefficient), decreasing = TRUE), ]
    
    # 设置颜色
    colors <- ifelse(df$Coefficient > 0, app_colors$success, app_colors$danger)
    
    # 创建条形图
    p <- plot_ly(df, x = ~Coefficient, y = ~Variable, type = "bar", 
               orientation = "h", marker = list(color = colors)) %>%
      layout(
        title = "回归系数",
        xaxis = list(title = "系数值"),
        yaxis = list(title = "", categoryorder = "array", 
                    categoryarray = df$Variable),
        showlegend = FALSE
      )
    
    return(p)
  })
} 