library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# 回归应用场景浏览器模块
applicationBrowserUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
        wellPanel(
          h4("选择应用领域"),
          selectInput(ns("app_area"), "应用领域:", 
                    choices = c("经济学" = "economics",
                                "房地产" = "realestate",
                                "市场营销" = "marketing",
                                "健康科学" = "health",
                                "环境科学" = "environment")),
          
          h4("模型设置"),
          checkboxInput(ns("show_ci"), "显示置信区间", TRUE),
          checkboxInput(ns("show_diagnostics"), "显示诊断图", FALSE),
          
          actionButton(ns("generate_new"), "生成新数据", 
                     class = "btn-secondary btn-block"),
          
          hr(),
          
          h4("应用领域描述"),
          htmlOutput(ns("area_description"))
        )
      ),
      column(9,
        tabsetPanel(
          tabPanel("回归图", 
                  plotOutput(ns("regression_plot"), height = "400px")),
          tabPanel("结果表", 
                  tableOutput(ns("regression_table"))),
          tabPanel("模型摘要", 
                  verbatimTextOutput(ns("model_summary"))),
          tabPanel("诊断图", 
                  plotOutput(ns("diagnostic_plot"), height = "500px"))
        ),
        
        hr(),
        
        h4("模型解释"),
        htmlOutput(ns("coef_interpretation"))
      )
    )
  )
}

# Server函数
applicationBrowserServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 获取当前应用领域数据
    app_data <- reactive({
      # 根据选择的应用领域生成数据
      if(is.null(input$app_area) || input$app_area == "") {
        return(NULL)
      }
      
      n_samples <- 100
      
      if(input$app_area == "economics") {
        data.frame(
          x = rnorm(n_samples, mean = 30, sd = 10),  # 工作时间
          y = rnorm(n_samples, mean = 2000, sd = 500)  # 收入
        ) %>%
          mutate(
            y = 500 + 50 * x + rnorm(n_samples, 0, 200),  # 线性关系加噪声
            label = c("工作时间(小时/周)", "收入(元/周)")
          )
      } else if(input$app_area == "realestate") {
        data.frame(
          x = rnorm(n_samples, mean = 100, sd = 30),  # 面积
          y = rnorm(n_samples, mean = 3000000, sd = 1000000)  # 房价
        ) %>%
          mutate(
            y = 1000000 + 20000 * x + rnorm(n_samples, 0, 500000),  # 线性关系加噪声 
            label = c("房屋面积(平方米)", "房价(元)")
          )
      } else if(input$app_area == "marketing") {
        data.frame(
          x = rnorm(n_samples, mean = 5000, sd = 1500),  # 广告支出
          y = rnorm(n_samples, mean = 50000, sd = 15000)  # 销售额
        ) %>%
          mutate(
            y = 10000 + 8 * x + rnorm(n_samples, 0, 10000),  # 线性关系加噪声
            label = c("广告支出(元)", "销售额(元)")
          )
      } else if(input$app_area == "health") {
        data.frame(
          x = rnorm(n_samples, mean = 25, sd = 5),  # BMI
          y = rnorm(n_samples, mean = 120, sd = 20)  # 收缩压
        ) %>%
          mutate(
            y = 70 + 2 * x + rnorm(n_samples, 0, 10),  # 线性关系加噪声
            label = c("BMI指数", "收缩压(mmHg)")
          )
      } else if(input$app_area == "environment") {
        data.frame(
          x = rnorm(n_samples, mean = 20, sd = 8),  # 温度
          y = rnorm(n_samples, mean = 50, sd = 20)  # 能源消耗
        ) %>%
          mutate(
            y = 10 + 2 * x + rnorm(n_samples, 0, 5),  # 线性关系加噪声
            label = c("温度(°C)", "能源消耗(kWh)")
          )
      }
    })
    
    # 存储线性回归模型
    lm_model <- reactive({
      req(app_data())
      if(is.null(app_data()) || nrow(app_data()) == 0) {
        return(NULL)
      }
      lm(y ~ x, data = app_data())
    })
    
    # 渲染回归图
    output$regression_plot <- renderPlot({
      req(app_data())
      
      if(is.null(lm_model())) {
        return(ggplot() + 
               labs(title = "无可用模型", 
                    subtitle = "请检查数据是否正确生成") + 
               theme_minimal(base_size = 14))
      }
      
      d <- app_data()
      model <- lm_model()
      
      # 预测值
      d$predicted <- predict(model)
      
      # 绘制回归图
      p <- ggplot(d, aes(x = x, y = y)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", formula = y ~ x, color = "blue", 
                   linewidth = 1.2, se = input$show_ci) +
        labs(
          title = paste("应用领域:", get_area_name(input$app_area)),
          subtitle = "线性回归拟合图",
          x = d$label[1],
          y = d$label[2]
        ) +
        theme_minimal(base_size = 14)
      
      if(!is.null(input$show_equation) && input$show_equation) {
        # 获取系数
        intercept <- round(coef(model)[1], 2)
        slope <- round(coef(model)[2], 2)
        
        # 系数符号处理
        slope_sign <- ifelse(slope >= 0, "+", "")
        
        # 添加回归方程
        eq_text <- paste0("y = ", intercept, slope_sign, slope, " * x")
        r2_text <- paste0("R² = ", round(summary(model)$r.squared, 4))
        
        p <- p + labs(caption = paste(eq_text, r2_text, sep = ", "))
      }
      
      p
    })
    
    # 渲染诊断图
    output$diagnostic_plot <- renderPlot({
      req(app_data(), lm_model())
      
      if(is.null(lm_model())) {
        return(ggplot() + 
               labs(title = "无可用模型", 
                    subtitle = "请检查数据是否正确生成") + 
               theme_minimal(base_size = 14))
      }
      
      if(!input$show_diagnostics) {
        return(ggplot() + 
               labs(title = "请勾选"显示诊断图"选项查看模型诊断") + 
               theme_minimal(base_size = 14))
      }
      
      d <- app_data()
      model <- lm_model()
      
      # 创建2x2诊断图
      par(mfrow = c(2, 2))
      plot(model)
    })
    
    # 渲染回归结果表
    output$regression_table <- renderTable({
      req(lm_model())
      
      if(is.null(lm_model())) {
        return(data.frame(message = "无可用模型"))
      }
      
      model <- lm_model()
      summary_model <- summary(model)
      
      # 系数表
      coef_table <- summary_model$coefficients
      
      # 格式化系数表
      formatted_table <- data.frame(
        "系数" = c("截距", "斜率"),
        "估计值" = round(coef_table[, 1], 4),
        "标准误" = round(coef_table[, 2], 4),
        "t值" = round(coef_table[, 3], 4),
        "p值" = round(coef_table[, 4], 4)
      )
      
      formatted_table
    })
    
    # 渲染模型摘要
    output$model_summary <- renderText({
      req(lm_model())
      
      if(is.null(lm_model())) {
        return("无可用模型")
      }
      
      model <- lm_model()
      summary_model <- summary(model)
      
      # R平方和调整后的R平方
      r_squared <- round(summary_model$r.squared, 4)
      adj_r_squared <- round(summary_model$adj.r.squared, 4)
      
      # F统计量
      f_stat <- round(summary_model$fstatistic[1], 4)
      f_p_value <- format.pval(pf(
        summary_model$fstatistic[1],
        summary_model$fstatistic[2],
        summary_model$fstatistic[3],
        lower.tail = FALSE
      ), digits = 4)
      
      # 整体模型性能摘要
      paste0(
        "模型拟合度:\n",
        "R平方 = ", r_squared, " (确定系数)\n",
        "调整R平方 = ", adj_r_squared, " (考虑预测变量数量后的调整)\n",
        "F统计量 = ", f_stat, ", p值 = ", f_p_value, " (整体模型显著性)"
      )
    })
    
    # 渲染应用领域描述
    output$area_description <- renderUI({
      req(input$app_area)
      
      area_descriptions <- list(
        economics = "经济学应用：研究工作时间与收入的关系，帮助理解劳动力市场动态。",
        realestate = "房地产应用：分析房屋面积与价格的关系，用于房产估值。",
        marketing = "市场营销应用：研究广告支出对销售额的影响，优化营销策略。",
        health = "健康科学应用：分析BMI指数与血压的关系，评估健康风险。",
        environment = "环境科学应用：研究温度与能源消耗的关系，用于能源规划。"
      )
      
      HTML(area_descriptions[[input$app_area]])
    })
    
    # 渲染系数解释
    output$coef_interpretation <- renderUI({
      req(lm_model())
      
      if(is.null(lm_model())) {
        return(HTML("<p>无法解释系数，无可用模型。</p>"))
      }
      
      model <- lm_model()
      coef_summary <- summary(model)$coefficients
      
      intercept <- coef_summary[1, 1]
      slope <- coef_summary[2, 1]
      p_value <- coef_summary[2, 4]
      
      # 确保截距和斜率不是NA
      if(is.na(intercept) || is.na(slope)) {
        return(HTML("<p>无法解释系数，模型参数不可用。</p>"))
      }
      
      # 截距解释
      intercept_text <- paste0("<b>截距 = ", round(intercept, 2), 
                              ":</b> 当", app_data()$label[1], "为0时，预期的", 
                              app_data()$label[2], "值。")
      
      # 斜率解释
      effect_direction <- ifelse(slope > 0, "增加", "减少")
      slope_text <- paste0("<b>斜率 = ", round(slope, 2), 
                          ":</b> 当", app_data()$label[1], "增加1个单位，", 
                          app_data()$label[2], "平均", effect_direction, " ", 
                          round(abs(slope), 2), "个单位。")
      
      # 统计显著性
      significance <- ifelse(p_value < 0.05, 
                           "这种关系在统计上是显著的 (p < 0.05)。", 
                           paste0("这种关系在统计上不显著 (p = ", round(p_value, 4), ")。"))
      
      # 决定系数解释
      r_squared <- summary(model)$r.squared
      r_squared_text <- paste0("<b>R² = ", round(r_squared, 4), 
                             ":</b> 这表明模型解释了约", 
                             round(r_squared * 100, 1), "%的", 
                             app_data()$label[2], "变异。")
      
      # 实际应用建议
      applications <- list(
        economics = "这个模型可以用于预测不同工作时间的预期收入，或者估计增加工作时间的边际收益。",
        realestate = "这个模型可以用于根据面积估算房屋价格，或者评估增加面积的边际价值。",
        marketing = "这个模型可以用于预测广告支出的销售回报，或者优化广告预算分配。",
        health = "这个模型可以用于评估BMI变化对血压的潜在影响，或者制定健康干预策略。",
        environment = "这个模型可以用于预测温度变化对能源需求的影响，或者制定能源使用计划。"
      )
      
      application_text <- paste0("<b>应用建议:</b> ", applications[[input$app_area]])
      
      HTML(paste(intercept_text, slope_text, significance, r_squared_text, application_text, sep = "<br><br>"))
    })
    
    # 辅助函数：获取应用领域名称
    get_area_name <- function(area_code) {
      area_names <- c(
        economics = "经济学",
        realestate = "房地产",
        marketing = "市场营销",
        health = "健康科学",
        environment = "环境科学"
      )
      
      return(area_names[area_code])
    }
  })
} 