# 辅助函数文件
# 实现exercise_server函数，用于基础练习应用

# exercise_server函数：处理编程练习的服务器逻辑
exercise_server <- function(id, handler_func) {
  # 使用shiny的moduleServer函数正确处理session
  shiny::moduleServer(id, function(input, output, session) {
    handler_func(input, output, session)
  })
}

# 这是一个标准的模块实现，使用shiny提供的机制来确保session对象正确传递
# 不再使用moduleServer和callModule，避免session参数问题
# 这是一个简化版实现，仅用于本应用中的练习功能 