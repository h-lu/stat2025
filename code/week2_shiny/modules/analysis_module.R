# 数据分析模块
# 此模块提供描述性统计计算和dplyr基本操作功能

analysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        box(
          title = "分析选项", width = NULL, status = "primary",
          radioButtons(ns("analysis_type"), "选择分析类型:",
                       choices = c("描述性统计" = "descriptive",
                                   "dplyr操作" = "dplyr"),
                       selected = "descriptive")
        )
      )
    ),
    
    # 根据选择的分析类型显示不同的UI
    conditionalPanel(
      condition = sprintf("input['%s'] == 'descriptive'", ns("analysis_type")),
      fluidRow(
        column(6,
          box(
            title = "描述性统计选项", width = NULL, status = "info",
            selectInput(ns("var_select"), "选择变量:", choices = NULL),
            checkboxGroupInput(ns("stats_select"), "选择统计量:",
                             choices = c("计数" = "count",
                                        "均值" = "mean",
                                        "中位数" = "median",
                                        "标准差" = "sd",
                                        "最小值" = "min",
                                        "最大值" = "max",
                                        "四分位数" = "quantiles",
                                        "缺失值" = "na")),
            actionButton(ns("calc_stats"), "计算统计量", class = "btn-primary")
          )
        ),
        column(6, 
          box(
            title = "结果", width = NULL, status = "success",
            verbatimTextOutput(ns("stats_result"))
          )
        )
      ),
      fluidRow(
        column(12,
          box(
            title = "数据分布可视化", width = NULL, status = "info",
            plotOutput(ns("dist_plot"), height = "300px")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'dplyr'", ns("analysis_type")),
      fluidRow(
        column(12,
          tabBox(
            title = "dplyr操作", width = NULL,
            # 选择列
            tabPanel("选择列",
              checkboxGroupInput(ns("select_columns"), "选择要保留的列:", choices = NULL),
              actionButton(ns("do_select"), "执行选择", class = "btn-primary"),
              hr(),
              h4("结果预览:"),
              DT::dataTableOutput(ns("select_result"))
            ),
            
            # 筛选行
            tabPanel("筛选行",
              selectInput(ns("filter_col"), "选择筛选列:", choices = NULL),
              uiOutput(ns("filter_condition_ui")),
              actionButton(ns("do_filter"), "执行筛选", class = "btn-primary"),
              hr(),
              h4("结果预览:"),
              DT::dataTableOutput(ns("filter_result"))
            ),
            
            # 排序
            tabPanel("排序",
              selectInput(ns("arrange_col"), "选择排序列:", choices = NULL),
              radioButtons(ns("arrange_order"), "排序方式:",
                         choices = c("升序" = "asc", "降序" = "desc"),
                         selected = "asc"),
              actionButton(ns("do_arrange"), "执行排序", class = "btn-primary"),
              hr(),
              h4("结果预览:"),
              DT::dataTableOutput(ns("arrange_result"))
            ),
            
            # 创建新列
            tabPanel("创建新列",
              textInput(ns("new_col_name"), "新列名称:", placeholder = "例如: 总分"),
              textAreaInput(ns("new_col_expr"), "R表达式:", 
                          placeholder = "例如: 数学成绩 + 英语成绩"),
              actionButton(ns("do_mutate"), "创建新列", class = "btn-primary"),
              hr(),
              h4("结果预览:"),
              DT::dataTableOutput(ns("mutate_result"))
            ),
            
            # 分组汇总
            tabPanel("分组汇总",
              selectInput(ns("group_col"), "选择分组列:", choices = NULL),
              selectInput(ns("summarise_col"), "选择汇总列:", choices = NULL),
              checkboxGroupInput(ns("summarise_func"), "选择汇总函数:",
                              choices = c("计数" = "n",
                                         "均值" = "mean",
                                         "中位数" = "median",
                                         "标准差" = "sd",
                                         "最小值" = "min",
                                         "最大值" = "max")),
              actionButton(ns("do_summarise"), "执行汇总", class = "btn-primary"),
              hr(),
              h4("结果预览:"),
              DT::dataTableOutput(ns("summarise_result"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        box(
          title = "R代码", width = NULL, status = "primary",
          verbatimTextOutput(ns("code_output")),
          p("学习提示: 以上是数据分析的R代码，可以复制到自己的R脚本中使用。")
        )
      )
    )
  )
}

analysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # 反应式值，存储分析结果和代码
    analysis_result <- reactiveVal(NULL)
    analysis_code <- reactiveVal("")
    
    # 当数据发生变化时，更新选择项
    observe({
      req(data())
      
      df <- data()
      col_names <- names(df)
      
      # 更新各种选择框
      updateSelectInput(session, "var_select", choices = col_names)
      updateCheckboxGroupInput(session, "select_columns", choices = col_names, selected = col_names[1:min(5, length(col_names))])
      updateSelectInput(session, "filter_col", choices = col_names)
      updateSelectInput(session, "arrange_col", choices = col_names)
      updateSelectInput(session, "group_col", choices = col_names)
      updateSelectInput(session, "summarise_col", choices = col_names)
    })
    
    # 根据选择的列类型，动态生成筛选条件UI
    output$filter_condition_ui <- renderUI({
      req(data(), input$filter_col)
      
      df <- data()
      col <- df[[input$filter_col]]
      
      if (is.numeric(col)) {
        # 数值型变量
        min_val <- min(col, na.rm = TRUE)
        max_val <- max(col, na.rm = TRUE)
        
        tagList(
          sliderInput(session$ns("filter_num_range"), "值范围:",
                     min = min_val, max = max_val,
                     value = c(min_val, max_val))
        )
      } else if (is.character(col) || is.factor(col)) {
        # 分类变量
        unique_vals <- unique(col)
        unique_vals <- unique_vals[!is.na(unique_vals)]
        
        tagList(
          checkboxGroupInput(session$ns("filter_cat_values"), "选择值:",
                           choices = unique_vals,
                           selected = unique_vals)
        )
      } else if (inherits(col, "Date") || inherits(col, "POSIXct")) {
        # 日期型变量
        min_date <- min(col, na.rm = TRUE)
        max_date <- max(col, na.rm = TRUE)
        
        tagList(
          dateRangeInput(session$ns("filter_date_range"), "日期范围:",
                        start = min_date, end = max_date)
        )
      } else if (is.logical(col)) {
        # 逻辑型变量
        tagList(
          radioButtons(session$ns("filter_logical"), "选择值:",
                      choices = c("TRUE" = "TRUE",
                                 "FALSE" = "FALSE",
                                 "所有" = "all"),
                      selected = "all")
        )
      } else {
        # 其他类型
        p("此类型变量暂不支持筛选条件设置")
      }
    })
    
    # ===== 描述性统计 =====
    
    # 计算描述性统计量
    observeEvent(input$calc_stats, {
      req(data(), input$var_select, input$stats_select)
      
      df <- data()
      var <- df[[input$var_select]]
      stats_result <- list()
      
      # 生成代码字符串
      code_lines <- c(
        paste0("# 计算变量 '", input$var_select, "' 的描述性统计量"),
        paste0("var <- data$", input$var_select)
      )
      
      # 计算各种统计量
      for (stat in input$stats_select) {
        if (stat == "count") {
          if (is.numeric(var) || is.logical(var) || is.factor(var) || is.character(var)) {
            stats_result$count <- length(var)
            code_lines <- c(code_lines, "# 计算数量", "length(var)")
          }
        } else if (stat == "mean") {
          if (is.numeric(var)) {
            stats_result$mean <- mean(var, na.rm = TRUE)
            code_lines <- c(code_lines, "# 计算均值", "mean(var, na.rm = TRUE)")
          }
        } else if (stat == "median") {
          if (is.numeric(var)) {
            stats_result$median <- median(var, na.rm = TRUE)
            code_lines <- c(code_lines, "# 计算中位数", "median(var, na.rm = TRUE)")
          }
        } else if (stat == "sd") {
          if (is.numeric(var)) {
            stats_result$sd <- sd(var, na.rm = TRUE)
            code_lines <- c(code_lines, "# 计算标准差", "sd(var, na.rm = TRUE)")
          }
        } else if (stat == "min") {
          if (is.numeric(var) || inherits(var, "Date")) {
            stats_result$min <- min(var, na.rm = TRUE)
            code_lines <- c(code_lines, "# 计算最小值", "min(var, na.rm = TRUE)")
          }
        } else if (stat == "max") {
          if (is.numeric(var) || inherits(var, "Date")) {
            stats_result$max <- max(var, na.rm = TRUE)
            code_lines <- c(code_lines, "# 计算最大值", "max(var, na.rm = TRUE)")
          }
        } else if (stat == "quantiles") {
          if (is.numeric(var)) {
            stats_result$quantiles <- quantile(var, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
            code_lines <- c(code_lines, "# 计算四分位数", "quantile(var, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)")
          }
        } else if (stat == "na") {
          stats_result$na_count <- sum(is.na(var))
          stats_result$na_percent <- paste0(round(sum(is.na(var)) / length(var) * 100, 2), "%")
          code_lines <- c(code_lines, "# 计算缺失值", 
                          "sum(is.na(var))",
                          "paste0(round(sum(is.na(var)) / length(var) * 100, 2), \"%\")")
        }
      }
      
      # 格式化结果为文本输出
      output_text <- ""
      if ("count" %in% names(stats_result)) {
        output_text <- paste0(output_text, "计数: ", stats_result$count, "\n")
      }
      if ("mean" %in% names(stats_result)) {
        output_text <- paste0(output_text, "均值: ", round(stats_result$mean, 4), "\n")
      }
      if ("median" %in% names(stats_result)) {
        output_text <- paste0(output_text, "中位数: ", stats_result$median, "\n")
      }
      if ("sd" %in% names(stats_result)) {
        output_text <- paste0(output_text, "标准差: ", round(stats_result$sd, 4), "\n")
      }
      if ("min" %in% names(stats_result)) {
        output_text <- paste0(output_text, "最小值: ", stats_result$min, "\n")
      }
      if ("max" %in% names(stats_result)) {
        output_text <- paste0(output_text, "最大值: ", stats_result$max, "\n")
      }
      if ("quantiles" %in% names(stats_result)) {
        output_text <- paste0(output_text, "四分位数:\n",
                            "  25%: ", stats_result$quantiles[1], "\n",
                            "  50%: ", stats_result$quantiles[2], "\n",
                            "  75%: ", stats_result$quantiles[3], "\n")
      }
      if ("na_count" %in% names(stats_result)) {
        output_text <- paste0(output_text, "缺失值数量: ", stats_result$na_count, "\n",
                            "缺失值比例: ", stats_result$na_percent, "\n")
      }
      
      analysis_result(output_text)
      analysis_code(paste(code_lines, collapse = "\n"))
    })
    
    # 绘制分布图
    output$dist_plot <- renderPlot({
      req(data(), input$var_select)
      
      df <- data()
      var <- df[[input$var_select]]
      
      par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
      
      if (is.numeric(var)) {
        # 直方图
        hist(var, main = paste0(input$var_select, "的直方图"),
            xlab = input$var_select, col = "lightblue", border = "white")
        
        # 箱线图
        boxplot(var, main = paste0(input$var_select, "的箱线图"),
               ylab = input$var_select, col = "lightgreen")
      } else if (is.factor(var) || is.character(var)) {
        # 条形图
        barplot(table(var), main = paste0(input$var_select, "的条形图"),
               xlab = input$var_select, col = "lightblue", las = 2)
        
        # 饼图
        pie(table(var), main = paste0(input$var_select, "的饼图"),
            col = rainbow(length(unique(var))))
      } else if (is.logical(var)) {
        # 逻辑型变量
        barplot(table(var), main = paste0(input$var_select, "的条形图"),
               xlab = input$var_select, col = c("red", "green"))
      } else {
        # 其他类型
        plot(1, type = "n", xlab = "", ylab = "", main = "此类型变量不支持可视化")
        text(1, 1, "不支持此类型变量的可视化", cex = 1.5)
      }
    })
    
    # ===== dplyr操作 =====
    
    # 执行select操作
    observeEvent(input$do_select, {
      req(data(), input$select_columns)
      
      df <- data()
      
      # 检查是否安装了dplyr
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        analysis_result("需要安装dplyr包才能执行此操作。请运行: install.packages('dplyr')")
        return()
      }
      
      # 执行select操作
      result <- df[, input$select_columns, drop = FALSE]
      
      # 生成代码
      code <- paste0(
        "# 使用dplyr的select函数选择列\n",
        "library(dplyr)\n",
        "result <- data %>% select(", paste(input$select_columns, collapse = ", "), ")"
      )
      
      # 更新结果
      analysis_result(result)
      analysis_code(code)
      
      # 显示结果预览
      output$select_result <- DT::renderDataTable({
        DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
      })
    })
    
    # 执行filter操作
    observeEvent(input$do_filter, {
      req(data(), input$filter_col)
      
      df <- data()
      col <- df[[input$filter_col]]
      
      # 检查是否安装了dplyr
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        analysis_result("需要安装dplyr包才能执行此操作。请运行: install.packages('dplyr')")
        return()
      }
      
      # 根据列类型执行不同的筛选
      if (is.numeric(col)) {
        req(input$filter_num_range)
        min_val <- input$filter_num_range[1]
        max_val <- input$filter_num_range[2]
        
        result <- df[col >= min_val & col <= max_val, , drop = FALSE]
        
        code <- paste0(
          "# 使用dplyr的filter函数筛选行（数值范围）\n",
          "library(dplyr)\n",
          "result <- data %>% filter(", input$filter_col, " >= ", min_val, 
          " & ", input$filter_col, " <= ", max_val, ")"
        )
      } else if (is.character(col) || is.factor(col)) {
        req(input$filter_cat_values)
        result <- df[col %in% input$filter_cat_values, , drop = FALSE]
        
        # 为字符型变量正确引用
        quoted_values <- paste0("\"", input$filter_cat_values, "\"", collapse = ", ")
        
        code <- paste0(
          "# 使用dplyr的filter函数筛选行（分类值）\n",
          "library(dplyr)\n",
          "result <- data %>% filter(", input$filter_col, " %in% c(", quoted_values, "))"
        )
      } else if (inherits(col, "Date") || inherits(col, "POSIXct")) {
        req(input$filter_date_range)
        start_date <- input$filter_date_range[1]
        end_date <- input$filter_date_range[2]
        
        result <- df[col >= start_date & col <= end_date, , drop = FALSE]
        
        code <- paste0(
          "# 使用dplyr的filter函数筛选行（日期范围）\n",
          "library(dplyr)\n",
          "result <- data %>% filter(", input$filter_col, " >= as.Date(\"", start_date, 
          "\") & ", input$filter_col, " <= as.Date(\"", end_date, "\"))"
        )
      } else if (is.logical(col)) {
        req(input$filter_logical)
        
        if (input$filter_logical == "all") {
          result <- df
          code <- paste0(
            "# 不进行筛选\n",
            "result <- data"
          )
        } else {
          logical_val <- as.logical(input$filter_logical)
          result <- df[col == logical_val, , drop = FALSE]
          
          code <- paste0(
            "# 使用dplyr的filter函数筛选行（逻辑值）\n",
            "library(dplyr)\n",
            "result <- data %>% filter(", input$filter_col, " == ", tolower(input$filter_logical), ")"
          )
        }
      } else {
        result <- df
        code <- "# 此类型变量暂不支持筛选"
      }
      
      # 更新结果
      analysis_result(result)
      analysis_code(code)
      
      # 显示结果预览
      output$filter_result <- DT::renderDataTable({
        DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
      })
    })
    
    # 执行arrange操作
    observeEvent(input$do_arrange, {
      req(data(), input$arrange_col, input$arrange_order)
      
      df <- data()
      
      # 检查是否安装了dplyr
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        analysis_result("需要安装dplyr包才能执行此操作。请运行: install.packages('dplyr')")
        return()
      }
      
      # 执行排序
      if (input$arrange_order == "asc") {
        result <- df[order(df[[input$arrange_col]]), , drop = FALSE]
        
        code <- paste0(
          "# 使用dplyr的arrange函数升序排序\n",
          "library(dplyr)\n",
          "result <- data %>% arrange(", input$arrange_col, ")"
        )
      } else {
        result <- df[order(df[[input$arrange_col]], decreasing = TRUE), , drop = FALSE]
        
        code <- paste0(
          "# 使用dplyr的arrange函数降序排序\n",
          "library(dplyr)\n",
          "result <- data %>% arrange(desc(", input$arrange_col, "))"
        )
      }
      
      # 更新结果
      analysis_result(result)
      analysis_code(code)
      
      # 显示结果预览
      output$arrange_result <- DT::renderDataTable({
        DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
      })
    })
    
    # 执行mutate操作
    observeEvent(input$do_mutate, {
      req(data(), input$new_col_name, input$new_col_expr)
      
      df <- data()
      
      # 检查是否安装了dplyr
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        analysis_result("需要安装dplyr包才能执行此操作。请运行: install.packages('dplyr')")
        return()
      }
      
      # 尝试执行表达式
      tryCatch({
        # 创建一个环境，包含数据框的列作为变量
        env <- list2env(df)
        
        # 在该环境中评估表达式
        new_col <- eval(parse(text = input$new_col_expr), envir = env)
        
        # 添加新列到数据框
        df[[input$new_col_name]] <- new_col
        
        code <- paste0(
          "# 使用dplyr的mutate函数创建新列\n",
          "library(dplyr)\n",
          "result <- data %>% mutate(", input$new_col_name, " = ", input$new_col_expr, ")"
        )
        
        # 更新结果
        analysis_result(df)
        analysis_code(code)
        
        # 显示结果预览
        output$mutate_result <- DT::renderDataTable({
          DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
        })
        
      }, error = function(e) {
        output$mutate_result <- DT::renderDataTable({
          data.frame(错误 = paste0("表达式错误: ", e$message))
        })
        
        analysis_code(paste0(
          "# 尝试创建新列，但表达式有错误\n",
          "# 错误信息: ", e$message, "\n",
          "# 原表达式: ", input$new_col_expr
        ))
      })
    })
    
    # 执行group_by和summarise操作
    observeEvent(input$do_summarise, {
      req(data(), input$group_col, input$summarise_col, input$summarise_func)
      
      df <- data()
      
      # 检查是否安装了dplyr
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        analysis_result("需要安装dplyr包才能执行此操作。请运行: install.packages('dplyr')")
        return()
      }
      
      # 创建存储结果的数据框
      group_vals <- unique(df[[input$group_col]])
      result <- data.frame(group = group_vals)
      names(result)[1] <- input$group_col
      
      # 生成代码
      code_parts <- c(
        "# 使用dplyr的group_by和summarise函数分组汇总",
        "library(dplyr)",
        paste0("result <- data %>% group_by(", input$group_col, ") %>% summarise(")
      )
      
      # 计算每个汇总函数
      summary_parts <- character(0)
      
      for (func in input$summarise_func) {
        if (func == "n") {
          result$n <- sapply(group_vals, function(g) {
            sum(df[[input$group_col]] == g, na.rm = TRUE)
          })
          summary_parts <- c(summary_parts, "n = n()")
        } else if (func == "mean" && is.numeric(df[[input$summarise_col]])) {
          result$mean <- sapply(group_vals, function(g) {
            mean(df[[input$summarise_col]][df[[input$group_col]] == g], na.rm = TRUE)
          })
          summary_parts <- c(summary_parts, paste0("mean_", input$summarise_col, " = mean(", input$summarise_col, ", na.rm = TRUE)"))
        } else if (func == "median" && is.numeric(df[[input$summarise_col]])) {
          result$median <- sapply(group_vals, function(g) {
            median(df[[input$summarise_col]][df[[input$group_col]] == g], na.rm = TRUE)
          })
          summary_parts <- c(summary_parts, paste0("median_", input$summarise_col, " = median(", input$summarise_col, ", na.rm = TRUE)"))
        } else if (func == "sd" && is.numeric(df[[input$summarise_col]])) {
          result$sd <- sapply(group_vals, function(g) {
            sd(df[[input$summarise_col]][df[[input$group_col]] == g], na.rm = TRUE)
          })
          summary_parts <- c(summary_parts, paste0("sd_", input$summarise_col, " = sd(", input$summarise_col, ", na.rm = TRUE)"))
        } else if (func == "min" && is.numeric(df[[input$summarise_col]])) {
          result$min <- sapply(group_vals, function(g) {
            min(df[[input$summarise_col]][df[[input$group_col]] == g], na.rm = TRUE)
          })
          summary_parts <- c(summary_parts, paste0("min_", input$summarise_col, " = min(", input$summarise_col, ", na.rm = TRUE)"))
        } else if (func == "max" && is.numeric(df[[input$summarise_col]])) {
          result$max <- sapply(group_vals, function(g) {
            max(df[[input$summarise_col]][df[[input$group_col]] == g], na.rm = TRUE)
          })
          summary_parts <- c(summary_parts, paste0("max_", input$summarise_col, " = max(", input$summarise_col, ", na.rm = TRUE)"))
        }
      }
      
      # 完成代码
      code <- paste0(
        paste(code_parts, collapse = "\n"),
        paste(summary_parts, collapse = ", "),
        ")\n"
      )
      
      # 更新结果
      analysis_result(result)
      analysis_code(code)
      
      # 显示结果预览
      output$summarise_result <- DT::renderDataTable({
        DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
      })
    })
    
    # 根据分析类型输出代码
    output$code_output <- renderText({
      analysis_code()
    })
    
    # 输出统计结果
    output$stats_result <- renderText({
      if (input$analysis_type == "descriptive" && !is.null(analysis_result())) {
        if (is.character(analysis_result())) {
          return(analysis_result())
        }
      }
      return("")
    })
  })
} 