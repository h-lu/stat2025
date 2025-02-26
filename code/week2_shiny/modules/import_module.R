# 数据导入模块
# 此模块包含数据导入相关的UI和服务器函数

importUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        box(
          title = "数据导入方法选择", width = NULL, status = "primary",
          radioButtons(ns("import_type"), "选择数据导入方式:",
                      choices = c("示例数据" = "sample",
                                  "CSV文件" = "csv", 
                                  "Excel文件" = "excel",
                                  "RDS文件" = "rds"),
                      selected = "sample")
        )
      )
    ),
    
    # 根据选择的导入方式显示不同的UI
    conditionalPanel(
      condition = sprintf("input['%s'] == 'sample'", ns("import_type")),
      fluidRow(
        column(12,
          box(
            title = "示例数据选择", width = NULL, status = "info",
            selectInput(ns("sample_dataset"), "选择示例数据集:",
                       choices = c("学生成绩数据" = "student_data",
                                   "电影数据" = "movie_data",
                                   "销售数据" = "sales_data")),
            actionButton(ns("load_sample"), "加载示例数据", 
                         icon = icon("database"), 
                         class = "btn-info")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'csv'", ns("import_type")),
      fluidRow(
        column(12,
          box(
            title = "CSV文件导入", width = NULL, status = "info",
            fileInput(ns("csv_file"), "选择CSV文件",
                     accept = c("text/csv", "text/comma-separated-values", 
                                ".csv")),
            checkboxInput(ns("header"), "文件包含列名", TRUE),
            radioButtons(ns("sep"), "分隔符:",
                        choices = c(逗号 = ",", 分号 = ";", Tab = "\t"),
                        selected = ","),
            radioButtons(ns("encoding"), "文件编码:",
                        choices = c(UTF8 = "UTF-8", GBK = "GBK"),
                        selected = "UTF-8"),
            actionButton(ns("load_csv"), "导入CSV文件", 
                         icon = icon("file-csv"), 
                         class = "btn-info")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'excel'", ns("import_type")),
      fluidRow(
        column(12,
          box(
            title = "Excel文件导入", width = NULL, status = "info",
            fileInput(ns("excel_file"), "选择Excel文件",
                     accept = c(".xlsx", ".xls")),
            numericInput(ns("sheet"), "工作表索引", 1, min = 1),
            checkboxInput(ns("excel_header"), "文件包含列名", TRUE),
            actionButton(ns("load_excel"), "导入Excel文件", 
                         icon = icon("file-excel"), 
                         class = "btn-info")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'rds'", ns("import_type")),
      fluidRow(
        column(12,
          box(
            title = "RDS文件导入", width = NULL, status = "info",
            fileInput(ns("rds_file"), "选择RDS文件",
                     accept = c(".rds", ".RDS")),
            actionButton(ns("load_rds"), "导入RDS文件", 
                         icon = icon("file"), 
                         class = "btn-info")
          )
        )
      )
    ),
    
    # 导入状态显示
    fluidRow(
      column(12,
        uiOutput(ns("import_status"))
      )
    ),
    
    # 导入代码展示
    fluidRow(
      column(12,
        box(
          title = "R代码", width = NULL, status = "primary",
          verbatimTextOutput(ns("import_code")),
          p("学习提示: 以上是导入数据的R代码，可以复制到自己的R脚本中使用。")
        )
      )
    )
  )
}

importServer <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    # 反应式值，存储导入的数据
    data_import <- reactiveVal(NULL)
    import_code <- reactiveVal("")
    import_message <- reactiveVal("")
    
    # 浏览示例数据源
    sample_data_paths <- list(
      csv = list(
        "学生成绩数据" = "student_data.csv",
        "电影数据" = "movie_data.csv",
        "销售数据" = "sales_data.csv"
      ),
      rds = list(
        "学生成绩数据" = "student_data.rds",
        "电影数据" = "movie_data.rds",
        "销售数据" = "sales_data.rds"
      )
    )
    
    # 导入示例数据
    observeEvent(input$load_sample, {
      req(input$sample_dataset)
      
      # 尝试加载数据
      tryCatch({
        # 构建路径 - 直接根据选择获取正确的文件名
        dataset_key <- input$sample_dataset
        file_path <- paste0(dataset_key, ".rds")
        
        # 检查文件是否存在
        if (!file.exists(file_path)) {
          stop(paste0("找不到文件: ", file_path))
        }
        
        # 加载数据
        data <- readRDS(file_path)
        
        # 更新反应式值
        data_import(data)
        
        # 设置导入代码
        if(input$sample_dataset == "student_data") {
          dataset_name <- "学生成绩数据"
        } else if(input$sample_dataset == "movie_data") {
          dataset_name <- "电影数据"
        } else {
          dataset_name <- "销售数据"
        }
        
        import_code(paste0('# 使用readRDS函数导入.rds文件\n',
                          'data <- readRDS("', file_path, '")\n',
                          '\n# 也可以使用CSV格式导入\n',
                          'library(readr)\n',
                          'data <- read_csv("', dataset_key, '.csv", locale = locale(encoding = "UTF-8"))\n'))
        
        import_message(paste0("成功导入", dataset_name, "，共", nrow(data), "行，", ncol(data), "列"))
        
      }, error = function(e) {
        import_message(paste0("导入数据出错: ", e$message))
        data_import(NULL)
      })
    })
    
    # 导入CSV文件
    observeEvent(input$load_csv, {
      req(input$csv_file)
      
      # 尝试导入CSV
      tryCatch({
        # 获取文件路径
        file_path <- input$csv_file$datapath
        
        # 导入数据
        if (input$encoding == "UTF-8") {
          data <- read.csv(file_path, header = input$header, sep = input$sep, fileEncoding = "UTF-8")
        } else {
          data <- read.csv(file_path, header = input$header, sep = input$sep, fileEncoding = "GBK")
        }
        
        # 更新反应式值
        data_import(data)
        
        # 设置导入代码
        import_code(paste0('# 使用基础R函数导入CSV\n',
                          'data <- read.csv("', input$csv_file$name, '", ',
                          'header = ', input$header, ', ',
                          'sep = "', input$sep, '", ',
                          'fileEncoding = "', input$encoding, '")\n\n',
                          '# 使用readr包导入CSV\n',
                          'library(readr)\n',
                          'data <- read_csv("', input$csv_file$name, '", ',
                          'col_names = ', input$header, ', ',
                          'locale = locale(encoding = "', input$encoding, '"))'))
        
        import_message(paste0("成功导入CSV文件，共", nrow(data), "行，", ncol(data), "列"))
        
      }, error = function(e) {
        import_message(paste0("导入CSV文件出错: ", e$message))
        data_import(NULL)
      })
    })
    
    # 导入Excel文件
    observeEvent(input$load_excel, {
      req(input$excel_file)
      
      # 尝试导入Excel
      tryCatch({
        # 加载readxl包
        if (!requireNamespace("readxl", quietly = TRUE)) {
          import_message("需要安装readxl包才能导入Excel文件。请运行: install.packages('readxl')")
          return()
        }
        
        # 获取文件路径
        file_path <- input$excel_file$datapath
        
        # 导入数据
        data <- readxl::read_excel(file_path, sheet = input$sheet, col_names = input$excel_header)
        
        # 更新反应式值
        data_import(data)
        
        # 设置导入代码
        import_code(paste0('# 使用readxl包导入Excel文件\n',
                          'library(readxl)\n',
                          'data <- read_excel("', input$excel_file$name, '", ',
                          'sheet = ', input$sheet, ', ',
                          'col_names = ', input$excel_header, ')'))
        
        import_message(paste0("成功导入Excel文件，共", nrow(data), "行，", ncol(data), "列"))
        
      }, error = function(e) {
        import_message(paste0("导入Excel文件出错: ", e$message))
        data_import(NULL)
      })
    })
    
    # 导入RDS文件
    observeEvent(input$load_rds, {
      req(input$rds_file)
      
      # 尝试导入RDS
      tryCatch({
        # 获取文件路径
        file_path <- input$rds_file$datapath
        
        # 导入数据
        data <- readRDS(file_path)
        
        # 更新反应式值
        data_import(data)
        
        # 设置导入代码
        import_code(paste0('# 使用readRDS函数导入RDS文件\n',
                          'data <- readRDS("', input$rds_file$name, '")'))
        
        import_message(paste0("成功导入RDS文件，共", nrow(data), "行，", ncol(data), "列"))
        
      }, error = function(e) {
        import_message(paste0("导入RDS文件出错: ", e$message))
        data_import(NULL)
      })
    })
    
    # 输出导入状态
    output$import_status <- renderUI({
      msg <- import_message()
      if (msg == "") return(NULL)
      
      if (grepl("成功", msg)) {
        div(class = "alert alert-success", icon("check"), msg)
      } else {
        div(class = "alert alert-danger", icon("exclamation-triangle"), msg)
      }
    })
    
    # 输出导入代码
    output$import_code <- renderText({
      import_code()
    })
    
    # 返回导入的数据
    return(data_import)
  })
} 