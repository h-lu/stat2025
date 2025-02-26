# 数据查看模块
# 此模块提供数据查看和初步理解的功能

viewUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        box(
          title = "数据概览", width = NULL, status = "primary",
          tabsetPanel(
            tabPanel("数据预览", 
                    DT::dataTableOutput(ns("data_preview"))
            ),
            tabPanel("数据结构", 
                    verbatimTextOutput(ns("str_output")),
                    verbatimTextOutput(ns("glimpse_output"))
            ),
            tabPanel("描述性统计", 
                    verbatimTextOutput(ns("summary_output"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12, 
        box(
          title = "数据质量分析", width = NULL, status = "primary",
          tabsetPanel(
            tabPanel("缺失值",
                    plotOutput(ns("missing_plot"), height = "300px"),
                    DT::dataTableOutput(ns("missing_table"))
            ),
            tabPanel("异常值检测",
                    selectInput(ns("outlier_var"), "选择变量:", choices = NULL),
                    plotOutput(ns("outlier_plot"), height = "300px"),
                    verbatimTextOutput(ns("outlier_stats"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12, 
        box(
          title = "R代码", width = NULL, status = "primary",
          verbatimTextOutput(ns("view_code")),
          p("学习提示: 以上是查看数据的R代码，可以复制到自己的R脚本中使用。")
        )
      )
    )
  )
}

viewServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # 更新outlier_var选择变量列表
    observe({
      req(data())
      
      # 获取数值型变量
      df <- data()
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      
      # 更新选择框
      updateSelectInput(session, "outlier_var", choices = numeric_vars)
    })
    
    # 数据预览
    output$data_preview <- DT::renderDataTable({
      req(data())
      DT::datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # 数据结构
    output$str_output <- renderPrint({
      req(data())
      str(data())
    })
    
    output$glimpse_output <- renderPrint({
      req(data())
      
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        return("需要安装dplyr包才能使用glimpse函数。请运行: install.packages('dplyr')")
      }
      
      dplyr::glimpse(data())
    })
    
    # 描述性统计
    output$summary_output <- renderPrint({
      req(data())
      summary(data())
    })
    
    # 缺失值分析
    output$missing_plot <- renderPlot({
      req(data())
      
      df <- data()
      missing_counts <- sapply(df, function(x) sum(is.na(x)))
      missing_percentages <- round(missing_counts / nrow(df) * 100, 1)
      
      # 创建条形图
      par(mar = c(7, 4, 2, 2))
      barplot(missing_percentages, 
              main = "各变量缺失值百分比", 
              las = 2, 
              cex.names = 0.8,
              col = ifelse(missing_percentages > 0, "salmon", "lightgreen"))
      abline(h = 5, lty = 2, col = "red")
      text(x = seq_along(missing_percentages) - 0.5, 
           y = missing_percentages + 1, 
           labels = paste0(missing_percentages, "%"), 
           cex = 0.7)
    })
    
    output$missing_table <- DT::renderDataTable({
      req(data())
      
      df <- data()
      missing_counts <- sapply(df, function(x) sum(is.na(x)))
      missing_percentages <- round(missing_counts / nrow(df) * 100, 1)
      
      missing_df <- data.frame(
        变量名 = names(df),
        缺失值数量 = missing_counts,
        缺失值比例 = paste0(missing_percentages, "%")
      )
      
      DT::datatable(missing_df, options = list(pageLength = 5))
    })
    
    # 异常值分析
    output$outlier_plot <- renderPlot({
      req(data(), input$outlier_var)
      
      df <- data()
      var <- input$outlier_var
      x <- df[[var]]
      
      # 去除NA值
      x <- x[!is.na(x)]
      
      par(mfrow = c(1, 2))
      
      # 箱线图
      boxplot(x, main = paste0(var, "的箱线图"), col = "lightblue")
      
      # 直方图
      hist(x, main = paste0(var, "的直方图"), 
           xlab = var, col = "lightgreen", 
           breaks = min(30, max(10, round(length(x) / 20))))
    })
    
    output$outlier_stats <- renderPrint({
      req(data(), input$outlier_var)
      
      df <- data()
      var <- input$outlier_var
      x <- df[[var]]
      
      # 去除NA值
      x <- x[!is.na(x)]
      
      # 计算四分位数和IQR
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      
      # 定义异常值边界
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      # 找出异常值
      outliers <- x[x < lower_bound | x > upper_bound]
      
      # 打印结果
      cat("变量:", var, "\n")
      cat("均值:", mean(x), "\n")
      cat("中位数:", median(x), "\n")
      cat("标准差:", sd(x), "\n")
      cat("最小值:", min(x), "\n")
      cat("第一四分位数(Q1):", q1, "\n")
      cat("第三四分位数(Q3):", q3, "\n")
      cat("四分位距(IQR):", iqr, "\n")
      cat("最大值:", max(x), "\n")
      cat("异常值边界: [", lower_bound, ",", upper_bound, "]\n")
      cat("异常值数量:", length(outliers), "\n")
      if (length(outliers) > 0 && length(outliers) <= 20) {
        cat("异常值:", outliers, "\n")
      } else if (length(outliers) > 20) {
        cat("异常值太多，只显示前20个:", head(outliers, 20), "...\n")
      }
    })
    
    # 查看代码
    output$view_code <- renderText({
      req(data())
      
      # 构建代码
      paste0(
        "# 查看数据的基本信息\n",
        "head(data)  # 查看前6行数据\n",
        "str(data)   # 查看数据结构\n\n",
        "# 使用dplyr包的glimpse函数查看数据\n",
        "library(dplyr)\n",
        "glimpse(data)\n\n",
        "# 计算描述性统计量\n",
        "summary(data)  # 所有变量的统计摘要\n\n",
        "# 检查缺失值\n",
        "colSums(is.na(data))  # 每列的缺失值数量\n",
        "colMeans(is.na(data)) * 100  # 每列的缺失值百分比\n\n",
        "# 异常值分析 (以", input$outlier_var, "为例)\n",
        "boxplot(data$", input$outlier_var, ")  # 箱线图\n",
        "hist(data$", input$outlier_var, ")  # 直方图\n\n",
        "# 计算四分位距和异常值\n",
        "q1 <- quantile(data$", input$outlier_var, ", 0.25, na.rm = TRUE)\n",
        "q3 <- quantile(data$", input$outlier_var, ", 0.75, na.rm = TRUE)\n",
        "iqr <- q3 - q1\n",
        "lower_bound <- q1 - 1.5 * iqr\n",
        "upper_bound <- q3 + 1.5 * iqr\n",
        "outliers <- data$", input$outlier_var, "[data$", input$outlier_var, " < lower_bound | data$", input$outlier_var, " > upper_bound]\n",
        "print(outliers)  # 打印异常值"
      )
    })
  })
} 