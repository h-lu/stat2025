library(shiny)
library(ggplot2)
library(vcd) # For mosaic plots

# Define UI
ui <- fluidPage(
    titlePanel("第七周交互式课件：分类数据分析"),
    
    tabsetPanel(
        # --- Tab 1: Goodness-of-Fit Test ---
        tabPanel("卡方拟合优度检验",
                 sidebarLayout(
                     sidebarPanel(
                         h4("输入数据"),
                         textInput("gof_observed", "观测频数 (用逗号分隔):", "85, 95, 110, 110"),
                         textInput("gof_expected_p", "期望比例 (用逗号分隔，可选，默认为均匀分布):", ""),
                         actionButton("gof_run", "执行检验"),
                         hr(),
                         h5("说明:"),
                         p("输入观测到的各类别的频数。如果期望比例不输入，则默认检验观测频数是否符合均匀分布。如果输入期望比例，请确保比例之和为1。")
                     ),
                     mainPanel(
                         h4("检验结果"),
                         verbatimTextOutput("gof_output"),
                         hr(),
                         h4("观测 vs 期望 频数可视化"),
                         plotOutput("gof_plot")
                     )
                 )
        ),
        
        # --- Tab 2: Independence Test ---
        tabPanel("卡方独立性检验",
                 sidebarLayout(
                     sidebarPanel(
                         h4("输入列联表数据"),
                         textAreaInput("indep_table", 
                                       "输入列联表 (用逗号分隔列，用换行分隔行):", 
                                       "150, 100\n200, 50", 
                                       rows = 4),
                         textInput("indep_rownames", "行名 (可选, 逗号分隔):", "男性, 女性"),
                         textInput("indep_colnames", "列名 (可选, 逗号分隔):", "购买, 不购买"),
                         actionButton("indep_run", "执行检验"),
                         hr(),
                         h5("说明:"),
                         p("输入列联表的观测频数。确保每行有相同数量的列。行名和列名是可选的，用于标签显示。")
                     ),
                     mainPanel(
                         h4("检验结果"),
                         verbatimTextOutput("indep_output"),
                         hr(),
                         h4("列联表可视化 (马赛克图)"),
                         plotOutput("indep_plot"),
                         hr(),
                         h4("观测频数 vs 期望频数"),
                         tableOutput("indep_tables_comp")
                     )
                 )
        ),
        
        # --- Tab 3: Correlation Analysis ---
        tabPanel("相关分析",
                 sidebarLayout(
                     sidebarPanel(
                         h4("输入两组数据"),
                         textAreaInput("cor_x", "变量 X 数据 (用逗号分隔):", "10, 15, 12, 18, 20, 14, 16, 11, 19, 13", rows = 3),
                         textAreaInput("cor_y", "变量 Y 数据 (用逗号分隔):", "80, 95, 88, 110, 120, 92, 105, 85, 115, 90", rows = 3),
                         radioButtons("cor_method", "选择相关系数:",
                                      choices = list("Pearson (适用于线性关系)" = "pearson", 
                                                     "Spearman (适用于单调关系)" = "spearman"),
                                      selected = "pearson"),
                         actionButton("cor_run", "执行检验"),
                         hr(),
                         h5("说明:"),
                         p("输入两个变量的数据，确保数据点数量相同。选择合适的相关系数进行计算。Pearson 适用于连续变量间的线性关系，Spearman 适用于有序变量或非线性单调关系。")
                     ),
                     mainPanel(
                         h4("检验结果"),
                         verbatimTextOutput("cor_output"),
                         hr(),
                         h4("散点图可视化"),
                         plotOutput("cor_plot")
                     )
                 )
        )
    )
)

# Define Server logic
server <- function(input, output, session) {

    # --- Server Logic for Goodness-of-Fit Test ---
    gof_results <- eventReactive(input$gof_run, {
        obs_str <- gsub("\\s+", "", input$gof_observed) # Remove whitespace
        if (obs_str == "") return(list(error = "请输入观测频数"))
        observed <- tryCatch(as.numeric(unlist(strsplit(obs_str, ","))), 
                             warning = function(w) NULL, error = function(e) NULL)
        if (is.null(observed) || any(is.na(observed)) || any(observed < 0)) {
            return(list(error = "观测频数必须是逗号分隔的非负数字"))
        }

        exp_p_str <- gsub("\\s+", "", input$gof_expected_p)
        expected_p <- NULL
        if (exp_p_str != "") {
            expected_p <- tryCatch(as.numeric(unlist(strsplit(exp_p_str, ","))),
                                   warning = function(w) NULL, error = function(e) NULL)
            if (is.null(expected_p) || any(is.na(expected_p)) || any(expected_p < 0)) {
                return(list(error = "期望比例必须是逗号分隔的非负数字"))
            }
            if (length(observed) != length(expected_p)) {
                return(list(error = "观测频数和期望比例的数量必须一致"))
            }
            if (abs(sum(expected_p) - 1) > 1e-6) {
                 return(list(error = paste0("期望比例之和 (", sum(expected_p), ") 不等于 1")))
            }
             # Rescale just in case of small floating point errors, chisq.test requires sum=1
            expected_p <- expected_p / sum(expected_p)
        } else {
             # Default to uniform distribution if no expected proportions are given
             expected_p <- rep(1/length(observed), length(observed))
        }
        
        # Perform the test
        test_result <- tryCatch(chisq.test(x = observed, p = expected_p), error = function(e) e)
        
        if (inherits(test_result, "error")) {
            return(list(error = paste("卡方检验出错:", test_result$message)))
        } else {
             # Calculate expected counts for plotting
             expected_counts <- sum(observed) * expected_p
             return(list(test = test_result, observed = observed, expected = expected_counts, 
                         names = names(observed) %||% paste("类别", 1:length(observed))))
        }
    })

    output$gof_output <- renderPrint({
        results <- gof_results()
        if (!is.null(results$error)) {
            cat("错误:\n", results$error)
        } else {
            print(results$test)
            if(any(results$expected < 5)) {
                 cat("\n警告: 存在期望频数小于 5 的情况，卡方检验结果可能不准确。\n")
            }
        }
    })

    output$gof_plot <- renderPlot({
        results <- gof_results()
        if (!is.null(results$error) || is.null(results$test)) return(NULL)
        
        df_plot <- data.frame(
            Category = factor(results$names, levels = results$names),
            Frequency = c(results$observed, results$expected),
            Type = factor(rep(c("观测频数", "期望频数"), each = length(results$observed)),
                          levels = c("观测频数", "期望频数"))
        )
        
        ggplot(df_plot, aes(x = Category, y = Frequency, fill = Type)) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8), width=0.7) +
            scale_fill_manual(values = c("观测频数" = "steelblue", "期望频数" = "orange")) +
            labs(title = "观测频数 vs 期望频数", x = "类别", y = "频数", fill = "类型") +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # --- Server Logic for Independence Test ---
    indep_results <- eventReactive(input$indep_run, {
        table_text <- input$indep_table
        if(table_text == "") return(list(error = "请输入列联表数据"))
        
        # Parse text into matrix
        rows <- strsplit(trimws(table_text), "\n")[[1]]
        mat_list <- lapply(rows, function(row) {
            vals <- tryCatch(as.numeric(unlist(strsplit(trimws(row), "[,\\s\t]+"))), # Split by comma, space, or tab
                             warning = function(w) NULL, error = function(e) NULL)
            if(any(is.na(vals))) return(NULL) # Handle non-numeric input in a row
            vals 
        })
        
        # Check for parsing errors or inconsistent column numbers
        if(any(sapply(mat_list, is.null))) return(list(error = "输入包含非数字字符"))
        col_counts <- sapply(mat_list, length)
        if(length(unique(col_counts)) > 1) return(list(error = "每行的列数必须相同"))
        if(col_counts[1] == 0) return(list(error = "未检测到有效的数字数据"))

        mat <- tryCatch(do.call(rbind, mat_list), error = function(e) NULL)
        if(is.null(mat)) return(list(error = "无法创建矩阵，请检查输入格式"))
        if(any(mat < 0)) return(list(error = "频数不能为负数"))
        
        # Add row/col names if provided
        rnames_str <- trimws(input$indep_rownames)
        cnames_str <- trimws(input$indep_colnames)
        rnames <- if (rnames_str != "") unlist(strsplit(rnames_str, ",")) else NULL
        cnames <- if (cnames_str != "") unlist(strsplit(cnames_str, ",")) else NULL
        
        if (!is.null(rnames) && length(rnames) != nrow(mat)) return(list(error = paste0("行名数量 (", length(rnames), ") 与矩阵行数 (", nrow(mat), ") 不匹配")))
        if (!is.null(cnames) && length(cnames) != ncol(mat)) return(list(error = paste0("列名数量 (", length(cnames), ") 与矩阵列数 (", ncol(mat), ") 不匹配")))
        
        rownames(mat) <- rnames
        colnames(mat) <- cnames

        # Perform the test
        test_result <- tryCatch(chisq.test(mat), error = function(e) e)

        if (inherits(test_result, "error")) {
            return(list(error = paste("卡方检验出错:", test_result$message)))
        } else {
             # Add observed and expected tables to results for display
            return(list(test = test_result, 
                        observed = round(test_result$observed, 2), 
                        expected = round(test_result$expected, 2),
                        residuals = round(test_result$residuals, 2),
                        table_data = mat)) # Pass the original matrix for mosaic plot
        }
    })

    output$indep_output <- renderPrint({
        results <- indep_results()
        if (!is.null(results$error)) {
            cat("错误:\n", results$error)
        } else {
            print(results$test)
             if(any(results$expected < 5)) {
                 cat("\n警告: 存在期望频数小于 5 的情况，卡方检验结果可能不准确。可考虑 Fisher 精确检验或合并类别。\n")
            }
        }
    })

    output$indep_plot <- renderPlot({
        results <- indep_results()
        if (!is.null(results$error) || is.null(results$test)) return(NULL)
        
        # Use vcd::mosaic for visualization
        tryCatch({
            # Use shading based on Pearson residuals
            mosaic(results$table_data, shade = TRUE, legend = TRUE, 
                   main = "独立性检验马赛克图 (颜色代表残差)",
                   labeling_args = list(gp_labels = gpar(fontsize = 12), 
                                        gp_varnames = gpar(fontsize = 14)))
        }, error = function(e) {
            plot(1, type="n", axes=FALSE, xlab="", ylab="", main = "无法生成马赛克图")
            text(1, 1, paste("错误:", e$message), col = "red")
        })
    })
    
    output$indep_tables_comp <- renderTable({
        results <- indep_results()
        if (!is.null(results$error) || is.null(results$test)) return(NULL)

        # Combine Observed, Expected, Residuals into a single display structure if possible
        # This is a bit complex to format nicely in a single table.
        # Let's just show Observed and Expected side-by-side for simplicity.
        # Or print them separately. Let's try separate print.
        
        # Cannot directly return multiple tables. Let's format into one.
        rows <- rownames(results$observed)
        cols <- colnames(results$observed)
        df_list <- list()
        for(r in 1:length(rows)){
            for(c in 1:length(cols)){
                 df_list[[length(df_list) + 1]] <- data.frame(
                     Row = rows[r],
                     Column = cols[c],
                     Observed = results$observed[r, c],
                     Expected = results$expected[r, c],
                     Residual = results$residuals[r, c]
                 )
            }
        }
        bind_rows(df_list)
        
    }, rownames = FALSE, digits = 2)


    # --- Server Logic for Correlation Analysis ---
    cor_results <- eventReactive(input$cor_run, {
        x_str <- gsub("\\s+", "", input$cor_x)
        y_str <- gsub("\\s+", "", input$cor_y)
        if (x_str == "" || y_str == "") return(list(error = "请输入变量 X 和 Y 的数据"))

        x <- tryCatch(as.numeric(unlist(strsplit(x_str, ","))), 
                      warning = function(w) NULL, error = function(e) NULL)
        y <- tryCatch(as.numeric(unlist(strsplit(y_str, ","))),
                      warning = function(w) NULL, error = function(e) NULL)

        if (is.null(x) || any(is.na(x))) return(list(error = "变量 X 必须是逗号分隔的数字"))
        if (is.null(y) || any(is.na(y))) return(list(error = "变量 Y 必须是逗号分隔的数字"))
        if (length(x) != length(y)) return(list(error = "变量 X 和 Y 的数据点数量必须相同"))
        if (length(x) < 3) return(list(error = "数据点过少 (至少需要3个) 无法进行检验"))

        method <- input$cor_method
        
        # Perform the test
        test_result <- tryCatch(cor.test(x, y, method = method), error = function(e) e)

        if (inherits(test_result, "error")) {
            return(list(error = paste("相关性检验出错:", test_result$message)))
        } else {
             return(list(test = test_result, x = x, y = y, method = method))
        }
    })

    output$cor_output <- renderPrint({
        results <- cor_results()
        if (!is.null(results$error)) {
            cat("错误:\n", results$error)
        } else {
            print(results$test)
            cat(paste("\n相关系数解释 (", results$method, "):\n", sep=""))
            r <- results$test$estimate
            if (abs(r) >= 0.7) { cat("强相关\n") }
            else if (abs(r) >= 0.4) { cat("中等强度相关\n") }
            else if (abs(r) >= 0.1) { cat("弱相关\n") }
            else { cat("极弱或无相关\n") }
        }
    })

    output$cor_plot <- renderPlot({
        results <- cor_results()
        if (!is.null(results$error) || is.null(results$test)) return(NULL)
        
        df_plot <- data.frame(X = results$x, Y = results$y)
        
        p <- ggplot(df_plot, aes(x = X, y = Y)) +
            geom_point(size = 3, alpha = 0.7, color="blue") +
            labs(title = paste("变量 X 与 Y 的散点图 (", results$method, "相关)", sep=""),
                 x = "变量 X", y = "变量 Y") +
            theme_minimal(base_size = 14)
            
        # Add smooth line based on method (optional)
        if (results$method == "pearson") {
            p <- p + geom_smooth(method = "lm", se = FALSE, color = "red") # Linear model for Pearson
        } else {
             p <- p + geom_smooth(method = "loess", se = FALSE, color = "darkgreen") # LOESS smoother for Spearman (non-linear okay)
        }
        
        print(p)
    })
    
    # Utility for names (if NULL)
    `%||%` <- function(a, b) if (!is.null(a)) a else b

}

# Run the application 
shinyApp(ui = ui, server = server) 