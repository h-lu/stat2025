# server_predict.R - 预测模块的服务器逻辑

# 定义预测模块的服务器逻辑
server_predict <- function(input, output, session) {
  # 监听构建预测模型按钮
  predict_model <- eventReactive(input$build_predict_model, {
    req(input$predict_x_vars)
    
    # 构建公式
    formula <- as.formula(paste("medv ~", paste(input$predict_x_vars, collapse = " + ")))
    
    # 拟合模型
    model <- lm(formula, data = Boston)
    
    return(model)
  })
  
  # 显示模型摘要
  output$predict_model_summary <- renderPrint({
    req(predict_model())
    summary(predict_model())
  })
  
  # 输入变量的界面生成
  output$predict_inputs_ui <- renderUI({
    req(predict_model())
    model <- predict_model()
    
    # 获取模型中的变量（不包括截距）
    vars <- names(coef(model))[-1]
    
    # 为每个变量创建数值输入
    input_list <- lapply(vars, function(var) {
      var_mean <- round(mean(Boston[[var]]), 2)
      var_min <- round(min(Boston[[var]]), 2)
      var_max <- round(max(Boston[[var]]), 2)
      
      # 创建数值输入，使用数据集的均值作为默认值
      numericInput(
        inputId = paste0("predict_input_", var),
        label = var,
        value = var_mean,
        min = var_min,
        max = var_max,
        step = 0.01
      )
    })
    
    # 将输入控件列表转换为一个 div
    div(
      input_list,
      actionButton(
        "make_prediction", 
        "进行预测", 
        icon = icon("calculator"),
        class = "btn-primary"
      )
    )
  })
  
  # 处理预测
  prediction_result <- eventReactive(input$make_prediction, {
    req(predict_model())
    model <- predict_model()
    
    # 获取模型变量
    vars <- names(coef(model))[-1]
    
    # 收集用户输入的数据
    new_data <- data.frame(matrix(NA, ncol = length(vars), nrow = 1))
    names(new_data) <- vars
    
    # 从输入控件中获取值
    for(var in vars) {
      input_value <- input[[paste0("predict_input_", var)]]
      if(is.null(input_value)) {
        return(list(
          status = "error",
          message = paste("请为变量", var, "提供有效的值")
        ))
      }
      new_data[[var]] <- input_value
    }
    
    # 进行预测
    prediction <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)
    
    # 返回结果
    list(
      status = "success",
      value = prediction[1, "fit"],
      lower = prediction[1, "lwr"],
      upper = prediction[1, "upr"],
      new_data = new_data
    )
  })
  
  # 显示预测结果
  output$prediction_result_ui <- renderUI({
    req(prediction_result())
    result <- prediction_result()
    
    if(result$status == "error") {
      return(
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          result$message
        )
      )
    }
    
    # 格式化预测结果
    div(
      class = "well prediction-result",
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
      
      h4("预测结果", style = "margin-top: 0;"),
      
      div(
        style = "font-size: 24px; margin: 15px 0;",
        "预测房价值: ",
        span(
          style = "font-weight: bold; color: #3498db;",
          paste0(round(result$value, 2), " 千美元")
        )
      ),
      
      div(
        style = "margin-bottom: 15px;",
        "95% 预测区间: ",
        span(
          style = "color: #2ecc71;",
          paste0(round(result$lower, 2), " - ", round(result$upper, 2), " 千美元")
        )
      ),
      
      # 显示输入数据
      div(
        h5("您输入的数据:"),
        renderTable({
          # 转置数据框以便于展示
          t_data <- as.data.frame(t(result$new_data))
          colnames(t_data) <- "值"
          rownames(t_data) <- names(result$new_data)
          t_data
        }, rownames = TRUE, colnames = TRUE, width = "100%")
      )
    )
  })
  
  # 预测与实际值的比较图
  output$predict_compare_plot <- renderPlotly({
    req(predict_model())
    model <- predict_model()
    
    # 获取预测值和实际值
    predictions <- fitted(model)
    actual <- Boston$medv
    
    # 创建数据框
    df <- data.frame(
      Actual = actual,
      Predicted = predictions,
      Residual = predictions - actual
    )
    
    # 创建散点图
    p <- plot_ly(df, x = ~Actual, y = ~Predicted, 
                type = "scatter", mode = "markers",
                marker = list(
                  color = ~abs(Residual),
                  colorscale = "Viridis",
                  size = 8,
                  opacity = 0.7,
                  colorbar = list(title = "残差绝对值")
                ),
                hoverinfo = "text",
                text = ~paste("实际值:", round(Actual, 2),
                             "<br>预测值:", round(Predicted, 2),
                             "<br>残差:", round(Residual, 2)))
    
    # 添加对角线
    range_val <- c(min(c(df$Actual, df$Predicted)), max(c(df$Actual, df$Predicted)))
    p <- p %>% add_trace(
      x = range_val,
      y = range_val,
      type = "scatter",
      mode = "lines",
      line = list(color = app_colors$danger, width = 2, dash = "dash"),
      showlegend = FALSE
    )
    
    # 设置布局
    p <- p %>% layout(
      title = "预测值 vs 实际值",
      xaxis = list(title = "实际房价（千美元）"),
      yaxis = list(title = "预测房价（千美元）"),
      showlegend = FALSE
    )
    
    return(p)
  })
  
  # 残差分布图
  output$predict_residual_plot <- renderPlotly({
    req(predict_model())
    model <- predict_model()
    
    # 获取残差
    residuals <- residuals(model)
    
    # 创建残差直方图
    p <- plot_ly(x = residuals, type = "histogram",
                marker = list(color = app_colors$primary, 
                             line = list(color = "white", width = 0.4)),
                opacity = 0.7,
                name = "残差频率")
    
    # 添加密度曲线
    curve_x <- seq(min(residuals), max(residuals), length.out = 100)
    curve_y <- dnorm(curve_x, mean = mean(residuals), sd = sd(residuals))
    
    p <- p %>% add_trace(
      x = curve_x,
      y = curve_y,
      type = "scatter",
      mode = "lines",
      line = list(color = app_colors$secondary, width = 2),
      yaxis = "y2",
      name = "正态分布"
    )
    
    # 设置布局
    p <- p %>% layout(
      title = "残差分布",
      xaxis = list(title = "残差"),
      yaxis = list(title = "频数"),
      yaxis2 = list(
        title = "密度",
        overlaying = "y",
        side = "right"
      ),
      barmode = "overlay",
      showlegend = TRUE
    )
    
    return(p)
  })
  
  # 变量选择方法输出
  # 逐步回归结果
  output$stepwise_result <- renderPrint({
    req(length(input$predict_x_vars) > 0)
    
    # 构建全模型
    full_formula <- as.formula("medv ~ .")
    full_model <- lm(full_formula, data = Boston)
    
    # 进行逐步回归
    step_model <- step(full_model, direction = "both", trace = FALSE)
    
    # 显示结果
    cat("逐步回归选择的模型:\n\n")
    summary(step_model)
  })
  
  # 最佳子集结果
  output$best_subset_result <- renderPrint({
    req(length(input$predict_x_vars) > 0)
    
    # 准备数据
    y <- Boston$medv
    x <- model.matrix(medv ~ ., Boston)[,-1]  # 移除截距
    
    # 执行最佳子集选择
    best_subset <- regsubsets(x, y, nvmax = 13)
    best_summary <- summary(best_subset)
    
    # 找出最优模型（基于BIC）
    which_min <- which.min(best_summary$bic)
    optimal_vars <- names(coef(best_subset, which_min))[-1]  # 移除截距
    
    # 显示结果
    cat("基于BIC的最佳子集:\n")
    cat("\n选择的变量 (", which_min, "个):\n", paste(optimal_vars, collapse = ", "), "\n\n")
    
    # 拟合并显示最佳模型的摘要
    if(length(optimal_vars) > 0) {
      best_formula <- as.formula(paste("medv ~", paste(optimal_vars, collapse = " + ")))
      best_model <- lm(best_formula, data = Boston)
      print(summary(best_model))
    }
  })
  
  # 最佳子集选择图
  output$best_subset_plot <- renderPlot({
    req(length(input$predict_x_vars) > 0)
    
    # 准备数据
    y <- Boston$medv
    x <- model.matrix(medv ~ ., Boston)[,-1]  # 移除截距
    
    # 执行最佳子集选择
    best_subset <- regsubsets(x, y, nvmax = 13)
    best_summary <- summary(best_subset)
    
    # 绘制BIC、Cp、调整R²图
    par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
    
    # BIC图
    plot(best_summary$bic, type = "b", xlab = "变量数", ylab = "BIC",
         col = app_colors$primary, lwd = 2, main = "BIC (越小越好)")
    points(which.min(best_summary$bic), min(best_summary$bic), 
           col = "red", pch = 16, cex = 2)
    
    # Cp图
    plot(best_summary$cp, type = "b", xlab = "变量数", ylab = "Cp",
         col = app_colors$secondary, lwd = 2, main = "Cp (越小越好)")
    points(which.min(best_summary$cp), min(best_summary$cp), 
           col = "red", pch = 16, cex = 2)
    
    # 调整R²图
    plot(best_summary$adjr2, type = "b", xlab = "变量数", ylab = "调整R²",
         col = app_colors$warning, lwd = 2, main = "调整R² (越大越好)")
    points(which.max(best_summary$adjr2), max(best_summary$adjr2), 
           col = "red", pch = 16, cex = 2)
  })
  
  # Lasso回归结果
  output$lasso_result <- renderPrint({
    req(length(input$predict_x_vars) > 0)
    
    # 准备数据
    x <- model.matrix(medv ~ ., Boston)[,-1]  # 移除截距
    y <- Boston$medv
    
    # 执行交叉验证的Lasso
    cv_lasso <- cv.glmnet(x, y, alpha = 1)
    
    # 提取最优lambda下的系数
    cat("Lasso回归 (λ =", round(cv_lasso$lambda.min, 4), ")选择的变量:\n\n")
    
    # 获取非零系数
    lasso_coef <- coef(cv_lasso, s = "lambda.min")
    lasso_coef <- lasso_coef[lasso_coef != 0,]
    
    # 打印非零系数
    print(lasso_coef)
    
    # 显示选择的变量数量
    cat("\n选择了", length(lasso_coef) - 1, "个变量 (不包括截距)\n")
  })
  
  # Lasso回归系数路径图
  output$lasso_plot <- renderPlot({
    req(length(input$predict_x_vars) > 0)
    
    # 准备数据
    x <- model.matrix(medv ~ ., Boston)[,-1]  # 移除截距
    y <- Boston$medv
    
    # 执行Lasso并获取系数路径
    lasso_model <- glmnet(x, y, alpha = 1)
    
    # 绘制系数路径
    plot(lasso_model, xvar = "lambda", label = TRUE, 
         col = rainbow(ncol(x)))
    
    # 添加最优lambda的垂直线
    cv_lasso <- cv.glmnet(x, y, alpha = 1)
    abline(v = log(cv_lasso$lambda.min), lty = 2)
    abline(v = log(cv_lasso$lambda.1se), lty = 2, col = "gray")
    
    # 添加图例
    legend("bottomright", 
           legend = c("最小MSE的λ", "1个标准误内的λ"),
           lty = 2, col = c("black", "gray"), cex = 0.8)
    
    title("Lasso系数路径", 
          sub = "垂直线表示交叉验证选择的最优λ值")
  })
} 