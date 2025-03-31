# ui_predict.R - 变量选择与预测模块的UI

# 定义变量选择与预测模块的UI
tabItem(
  tabName = "predict",
  
  fluidRow(
    box(
      title = "变量选择与模型构建",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      selectizeInput(
        "predict_x_vars",
        "选择解释变量（可多选）",
        choices = setdiff(names(Boston), "medv"),
        multiple = TRUE
      ),
      
      actionButton(
        "build_predict_model",
        "构建预测模型",
        class = "btn-primary"
      )
    )
  ),
  
  fluidRow(
    box(
      title = "模型评估",
      status = "info",
      width = 12,
      verbatimTextOutput("predict_model_summary")
    )
  ),
  
  fluidRow(
    box(
      title = "预测输入",
      status = "success",
      width = 12,
      uiOutput("predict_inputs_ui")
    )
  ),
  
  fluidRow(
    box(
      title = "预测结果",
      status = "warning",
      width = 12,
      uiOutput("prediction_result_ui")
    )
  )
) 