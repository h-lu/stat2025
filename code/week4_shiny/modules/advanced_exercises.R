# 综合练习模块
# 包含推断统计的综合应用练习题

# UI函数
advancedExercisesUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      create_title_card(
        title = "综合练习",
        subtitle = "应用推断统计的方法解决实际问题"
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "练习说明",
        status = "info",
        solidHeader = TRUE,
        p("本模块包含一系列综合练习题，旨在帮助您应用推断统计的方法解决实际问题。这些练习题比基础练习更为复杂，需要综合运用多种统计方法。"),
        p("每道题目都提供了真实数据集，并要求您进行完整的统计分析，包括：设置假设、选择适当的检验方法、分析结果、计算效应量等。"),
        p("建议先尝试独立分析，然后再查看提示或解答。您也可以使用主程序中的t检验模块辅助解决这些问题。")
      )
    ),
    
    # 练习题选择器
    fluidRow(
      box(
        width = 12,
        title = "选择练习",
        status = "primary",
        solidHeader = TRUE,
        
        selectInput(
          ns("selected_exercise"),
          "选择练习题:",
          choices = lapply(advanced_exercises, function(ex) ex$title),
          selected = advanced_exercises[[1]]$title
        )
      )
    ),
    
    # 练习题内容
    fluidRow(
      uiOutput(ns("exercise_content"))
    ),
    
    # 数据分析工具箱
    fluidRow(
      conditionalPanel(
        condition = "input.show_analysis_tools == true",
        ns = ns,
        
        box(
          width = 12,
          title = "数据分析工具箱",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          
          fluidRow(
            column(
              width = 6,
              h4("描述性统计"),
              verbatimTextOutput(ns("descriptive_stats")),
              plotOutput(ns("descriptive_plot"), height = "300px")
            ),
            column(
              width = 6,
              h4("检验结果"),
              radioButtons(
                ns("analysis_type"),
                "选择分析类型:",
                choices = list(
                  "配对样本t检验" = "paired",
                  "独立样本t检验" = "two.sample",
                  "单样本t检验" = "one.sample"
                ),
                selected = "paired"
              ),
              
              radioButtons(
                ns("alternative_direction"),
                "备择假设方向:",
                choices = list(
                  "双尾 (≠)" = "two.sided",
                  "右尾 (>)" = "greater",
                  "左尾 (<)" = "less"
                ),
                selected = "two.sided"
              ),
              
              actionButton(
                ns("run_analysis"),
                "运行分析",
                icon = icon("play"),
                class = "btn-success"
              ),
              
              conditionalPanel(
                condition = "input.run_analysis",
                ns = ns,
                div(
                  style = "margin-top: 20px;",
                  verbatimTextOutput(ns("analysis_result"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# 服务器函数
advancedExercisesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 当前选择的练习题
    current_exercise <- reactive({
      ex_title <- input$selected_exercise
      for (ex in advanced_exercises) {
        if (ex$title == ex_title) {
          return(ex)
        }
      }
      return(NULL)
    })
    
    # 显示选中的练习题内容
    output$exercise_content <- renderUI({
      ns <- session$ns
      ex <- current_exercise()
      
      if (is.null(ex)) {
        return(NULL)
      }
      
      # 获取练习题数据
      data <- ex$data
      
      box(
        width = 12,
        title = paste0("练习: ", ex$title),
        status = "primary",
        solidHeader = TRUE,
        
        div(
          h4("问题描述:"),
          p(ex$description),
          
          # 显示数据预览
          fluidRow(
            column(
              width = 6,
              h4("数据预览 (组1/前测):"),
              p(paste(head(data$before, 10), collapse = ", "), "...")
            ),
            column(
              width = 6,
              if (length(data) > 1) {
                tagList(
                  h4("数据预览 (组2/后测):"),
                  p(paste(head(data$after, 10), collapse = ", "), "...")
                )
              }
            )
          ),
          
          # 操作按钮
          div(
            style = "margin-top: 20px;",
            actionButton(
              ns("show_hint"),
              "显示提示",
              icon = icon("lightbulb"),
              class = "btn-info"
            ),
            actionButton(
              ns("show_solution"),
              "显示解答",
              icon = icon("eye"),
              class = "btn-primary"
            ),
            actionButton(
              ns("show_analysis_tools"),
              "使用分析工具",
              icon = icon("tools"),
              class = "btn-success"
            )
          ),
          
          # 条件显示提示
          conditionalPanel(
            condition = "input.show_hint",
            ns = ns,
            div(
              class = "alert alert-info",
              style = "margin-top: 20px;",
              h4("提示:"),
              p(ex$hints)
            )
          ),
          
          # 条件显示解答
          conditionalPanel(
            condition = "input.show_solution",
            ns = ns,
            div(
              class = "alert alert-success",
              style = "margin-top: 20px;",
              h4("解答:"),
              p(ex$solution)
            )
          )
        )
      )
    })
    
    # 显示描述性统计结果
    output$descriptive_stats <- renderText({
      ex <- current_exercise()
      if (is.null(ex)) {
        return("请先选择一个练习题")
      }
      
      data <- ex$data
      
      # 根据数据结构生成不同的描述性统计
      if (all(c("before", "after") %in% names(data))) {
        # 配对数据
        group1 <- data$before
        group2 <- data$after
        diff <- group1 - group2
        
        paste0(
          "组1/前测:\n",
          "  样本量: ", length(group1), "\n",
          "  均值: ", round(mean(group1), 2), "\n",
          "  标准差: ", round(sd(group1), 2), "\n",
          "  最小值: ", round(min(group1), 2), "\n",
          "  最大值: ", round(max(group1), 2), "\n\n",
          "组2/后测:\n",
          "  样本量: ", length(group2), "\n",
          "  均值: ", round(mean(group2), 2), "\n",
          "  标准差: ", round(sd(group2), 2), "\n",
          "  最小值: ", round(min(group2), 2), "\n",
          "  最大值: ", round(max(group2), 2), "\n\n",
          "差异 (前测 - 后测):\n",
          "  均值: ", round(mean(diff), 2), "\n",
          "  标准差: ", round(sd(diff), 2)
        )
      } else if (all(c("drug_a", "drug_b") %in% names(data))) {
        # 药物治疗数据
        group1 <- data$drug_a
        group2 <- data$drug_b
        
        paste0(
          "药物A:\n",
          "  样本量: ", length(group1), "\n",
          "  均值: ", round(mean(group1), 2), "\n",
          "  标准差: ", round(sd(group1), 2), "\n",
          "  最小值: ", round(min(group1), 2), "\n",
          "  最大值: ", round(max(group1), 2), "\n\n",
          "药物B:\n",
          "  样本量: ", length(group2), "\n",
          "  均值: ", round(mean(group2), 2), "\n",
          "  标准差: ", round(sd(group2), 2), "\n",
          "  最小值: ", round(min(group2), 2), "\n",
          "  最大值: ", round(max(group2), 2), "\n\n",
          "均值差异 (A - B): ", round(mean(group1) - mean(group2), 2)
        )
      } else {
        return("不支持当前数据结构的描述性统计。")
      }
    })
    
    # 显示描述性统计图表
    output$descriptive_plot <- renderPlot({
      ex <- current_exercise()
      if (is.null(ex)) {
        return(NULL)
      }
      
      data <- ex$data
      
      # 根据数据结构生成不同的图表
      if (all(c("before", "after") %in% names(data))) {
        # 配对数据 - 箱线图和配对线条
        plot_data <- data.frame(
          value = c(data$before, data$after),
          group = factor(c(rep("前测", length(data$before)), rep("后测", length(data$after))))
        )
        
        ggplot() +
          # 箱线图
          geom_boxplot(data = plot_data, aes(x = group, y = value, fill = group)) +
          # 个体连线
          geom_line(data = data.frame(
            x = rep(c(1, 2), each = length(data$before)),
            y = c(rbind(data$before, data$after)),
            id = rep(1:length(data$before), 2)
          ), aes(x = x, y = y, group = id), color = "gray50", alpha = 0.5) +
          # 均值点
          stat_summary(data = plot_data, aes(x = group, y = value, color = group),
                      fun = mean, geom = "point", size = 4) +
          # 设置颜色
          scale_fill_manual(values = c("前测" = app_colors$primary, "后测" = app_colors$secondary)) +
          scale_color_manual(values = c("前测" = app_colors$primary, "后测" = app_colors$secondary)) +
          # 设置标题和轴标签
          labs(title = "前后测数据比较",
               x = "", y = "值") +
          theme_minimal() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(family = "notosans")
          )
      } else if (all(c("drug_a", "drug_b") %in% names(data))) {
        # 药物治疗数据 - 密度图
        plot_data <- data.frame(
          value = c(data$drug_a, data$drug_b),
          group = factor(c(rep("药物A", length(data$drug_a)), rep("药物B", length(data$drug_b))))
        )
        
        ggplot(plot_data, aes(x = value, fill = group)) +
          # 添加密度曲线
          geom_density(alpha = 0.7) +
          # 添加均值线
          geom_vline(xintercept = mean(data$drug_a), color = app_colors$chart_primary, linetype = "dashed", linewidth = 1.2) +
          geom_vline(xintercept = mean(data$drug_b), color = app_colors$chart_secondary, linetype = "dashed", linewidth = 1.2) +
          # 设置颜色
          scale_fill_manual(values = c("药物A" = app_colors$chart_primary, "药物B" = app_colors$chart_secondary)) +
          # 设置标题和轴标签
          labs(title = "药物A和药物B效果比较",
               subtitle = paste0("药物A均值: ", round(mean(data$drug_a), 2), ", 药物B均值: ", round(mean(data$drug_b), 2)),
               x = "值", y = "密度") +
          # 应用现代主题
          theme_modern() +
          theme(
            legend.position = "bottom"
          )
      } else {
        # 不支持的数据结构
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "不支持当前数据结构的图表可视化。") +
          theme_void()
      }
    })
    
    # 运行统计分析并显示结果
    observeEvent(input$run_analysis, {
      ex <- current_exercise()
      if (is.null(ex)) {
        return(NULL)
      }
      
      data <- ex$data
      analysis_type <- input$analysis_type
      alternative <- input$alternative_direction
      
      # 根据分析类型和数据结构执行不同的分析
      if (analysis_type == "paired" && all(c("before", "after") %in% names(data))) {
        # 配对样本t检验
        result <- run_paired_t_test(
          group1 = data$before,
          group2 = data$after,
          conf_level = 0.95,
          alternative = alternative
        )
      } else if (analysis_type == "two.sample" && 
                (all(c("before", "after") %in% names(data)) || 
                 all(c("drug_a", "drug_b") %in% names(data)))) {
        # 独立样本t检验
        if (all(c("before", "after") %in% names(data))) {
          result <- run_two_sample_t_test(
            group1 = data$before,
            group2 = data$after,
            conf_level = 0.95,
            alternative = alternative,
            var_equal = FALSE
          )
        } else {
          result <- run_two_sample_t_test(
            group1 = data$drug_a,
            group2 = data$drug_b,
            conf_level = 0.95,
            alternative = alternative,
            var_equal = FALSE
          )
        }
      } else if (analysis_type == "one.sample" && 
                all(c("before") %in% names(data))) {
        # 单样本t检验 (例如，与特定值比较)
        result <- run_one_sample_t_test(
          data = data$before,
          mu = mean(data$after),  # 使用后测均值作为参考值
          conf_level = 0.95,
          alternative = alternative
        )
      } else {
        return("所选分析类型与数据结构不匹配。请选择适当的分析类型。")
      }
      
      # 格式化结果
      output$analysis_result <- renderText({
        format_t_test_result(result)
      })
    })
    
    # 重置条件面板
    observeEvent(input$selected_exercise, {
      # 重置各种"显示"按钮的状态
      updateActionButton(session, "show_hint", label = "显示提示")
      updateActionButton(session, "show_solution", label = "显示解答")
      updateActionButton(session, "show_analysis_tools", label = "使用分析工具")
      
      # 清除分析结果
      output$analysis_result <- renderText("")
    })
  })
} 