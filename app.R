# app.R - Shiny应用程序主文件

# 加载全局变量和依赖
source("global.R")

# 加载工具函数
source("utils/data_utils.R")
source("utils/model_utils.R")
source("utils/plot_utils.R")

# 加载UI组件
source("ui/ui_main.R")
source("ui/ui_base.R")
source("ui/ui_diag.R")
source("ui/ui_multi.R")
source("ui/ui_predict.R")

# 定义UI
ui <- ui_main

# 定义服务器逻辑
server <- function(input, output, session) {
  # 数据预览和描述性统计
  output$boston_dims <- renderText({
    paste0("数据集维度: ", boston_overview$dims[1], "行 × ", boston_overview$dims[2], "列")
  })
  
  output$boston_preview <- DT::renderDataTable({
    DT::datatable(
      Boston, 
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  output$boston_dict <- DT::renderDataTable({
    DT::datatable(
      boston_dict,
      options = list(
        pageLength = 14,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # 基础回归模块
  
  # 数据预览
  output$data_preview <- DT::renderDataTable({
    DT::datatable(
      Boston,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        htmltools::em('波士顿房价数据集')
      )
    )
  })
  
  # 生成散点图和回归线
  output$base_scatter_plot <- renderPlotly({
    req(input$base_x_var)
    
    # 获取模型
    model <- get_simple_lm(input$base_x_var)
    
    if(is.null(model)) {
      return(NULL)
    }
    
    # 使用手动调整的参数或实际模型参数
    if(input$manual_intercept == coef(model)[1] && 
       input$manual_slope == coef(model)[2]) {
      # 使用实际模型参数
      plot_scatter_with_regression(input$base_x_var)
    } else {
      # 使用手动调整的参数
      plt <- plot_scatter_with_regression(input$base_x_var, show_line = FALSE)
      
      # 添加手动回归线
      x_range <- range(Boston[[input$base_x_var]])
      x_vals <- seq(x_range[1], x_range[2], length.out = 100)
      y_vals <- input$manual_intercept + input$manual_slope * x_vals
      
      # 添加手动线
      plt <- plt %>% 
        add_trace(
          x = x_vals,
          y = y_vals,
          type = "scatter",
          mode = "lines",
          line = list(color = app_colors$warning, width = 2),
          name = "手动调整线",
          hoverinfo = "none"
        )
      plt
    }
  })
  
  # 诊断模块
  
  # 生成模型
  diag_model <- reactive({
    req(input$diag_x_var)
    
    # 构建公式
    formula <- as.formula(paste("medv ~", input$diag_x_var))
    
    # 拟合模型
    model <- lm(formula, data = Boston)
    
    return(model)
  })
  
  # 基本回归图
  output$diag_basic_plot <- renderPlotly({
    req(diag_model())
    
    plot_scatter_with_regression(input$diag_x_var)
  })
  
  # 多元回归模块
  
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
  
  # 预测模块
  
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
}

# 运行应用程序
shinyApp(ui = ui, server = server) 