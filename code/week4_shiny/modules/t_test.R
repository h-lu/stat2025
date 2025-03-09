# t检验模块
# 演示三种t检验的应用和结果解释

# UI函数
tTestUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      create_title_card(
        title = "t检验演示",
        subtitle = "学习三种t检验: 单样本t检验、独立样本t检验和配对样本t检验"
      )
    ),
    
    fluidRow(
      box(
        width = 4,
        title = "检验设置",
        status = "primary",
        solidHeader = TRUE,
        
        # 检验类型选择
        radioButtons(
          ns("test_type"),
          "选择检验类型:",
          choices = list(
            "单样本t检验" = "one.sample",
            "独立样本t检验" = "two.sample",
            "配对样本t检验" = "paired"
          ),
          selected = "one.sample"
        ),
        
        # 数据源选择
        radioButtons(
          ns("data_source"),
          "数据源:",
          choices = list(
            "示例数据" = "example",
            "自定义数据" = "custom"
          ),
          selected = "example"
        ),
        
        # 示例数据选择（条件性显示）
        conditionalPanel(
          condition = "input.data_source == 'example'",
          ns = ns,
          
          selectInput(
            ns("example_dataset"),
            "选择示例数据:",
            choices = list(
              "电影评分" = "movie_ratings",
              "学生成绩" = "student_scores",
              "减肥计划" = "weight_loss",
              "药物治疗" = "treatment_effect"
            ),
            selected = "movie_ratings"
          )
        ),
        
        # 自定义数据输入（条件性显示）
        conditionalPanel(
          condition = "input.data_source == 'custom'",
          ns = ns,
          
          # 单样本t检验自定义数据
          conditionalPanel(
            condition = "input.test_type == 'one.sample'",
            ns = ns,
            
            textAreaInput(
              ns("custom_data1"),
              "输入数据 (用逗号分隔):",
              value = "4.5, 4.2, 3.8, 4.7, 4.0, 4.3, 3.9, 4.6, 4.1, 4.4",
              height = "100px"
            ),
            
            numericInput(
              ns("null_value"),
              "原假设值 (μ₀):",
              value = 4.0,
              step = 0.1
            )
          ),
          
          # 独立样本t检验自定义数据
          conditionalPanel(
            condition = "input.test_type == 'two.sample'",
            ns = ns,
            
            textAreaInput(
              ns("custom_data1_two"),
              "组1数据 (用逗号分隔):",
              value = "4.5, 4.2, 3.8, 4.7, 4.0, 4.3, 3.9, 4.6, 4.1, 4.4",
              height = "80px"
            ),
            
            textAreaInput(
              ns("custom_data2_two"),
              "组2数据 (用逗号分隔):",
              value = "3.8, 4.1, 3.6, 4.2, 4.0, 3.9, 4.3, 4.0, 3.7, 4.5",
              height = "80px"
            ),
            
            checkboxInput(
              ns("var_equal"),
              "假设两组方差相等",
              value = FALSE
            )
          ),
          
          # 配对样本t检验自定义数据
          conditionalPanel(
            condition = "input.test_type == 'paired'",
            ns = ns,
            
            textAreaInput(
              ns("custom_data1_paired"),
              "前测数据 (用逗号分隔):",
              value = "68, 72, 65, 82, 75, 70, 64, 83, 76, 79",
              height = "80px"
            ),
            
            textAreaInput(
              ns("custom_data2_paired"),
              "后测数据 (用逗号分隔):",
              value = "73, 76, 70, 85, 79, 75, 69, 88, 81, 84",
              height = "80px"
            )
          )
        ),
        
        # 备择假设方向
        radioButtons(
          ns("alternative"),
          "备择假设方向:",
          choices = list(
            "双尾 (≠)" = "two.sided",
            "右尾 (>)" = "greater",
            "左尾 (<)" = "less"
          ),
          selected = "two.sided"
        ),
        
        # 置信水平
        sliderInput(
          ns("conf_level"),
          "置信水平:",
          min = 0.8,
          max = 0.99,
          value = 0.95,
          step = 0.01
        ),
        
        # 运行检验按钮
        actionButton(
          ns("run_test"),
          "运行t检验",
          icon = icon("play"),
          class = "btn-primary btn-block"
        )
      ),
      
      # 检验结果展示
      box(
        width = 8,
        title = "t检验结果",
        status = "primary",
        solidHeader = TRUE,
        
        tabsetPanel(
          tabPanel(
            "可视化",
            plotOutput(ns("test_plot"), height = "400px")
          ),
          tabPanel(
            "数据摘要",
            div(
              style = "margin-top: 20px;",
              h4("描述性统计:"),
              verbatimTextOutput(ns("data_summary"))
            )
          ),
          tabPanel(
            "检验结果",
            div(
              style = "margin-top: 20px;",
              h4("t检验结果:"),
              verbatimTextOutput(ns("test_result"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "t检验概念解释",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("什么是t检验?"),
        p("t检验是一种常用的参数检验方法，用于比较均值是否存在显著差异。t检验基于t分布，特别适用于样本量较小且总体标准差未知的情况。"),
        
        h4("t检验的三种主要类型"),
        tags$ul(
          tags$li(
            strong("单样本t检验 (One-Sample t-test):"),
            "用于检验一个样本的均值是否与已知或假设的总体均值存在显著差异。",
            tags$ul(
              tags$li("原假设 H₀: μ = μ₀ (样本均值等于特定值)"),
              tags$li("备择假设 H₁: μ ≠ μ₀ (样本均值不等于特定值) - 双尾检验"),
              tags$li("或 H₁: μ > μ₀ (样本均值大于特定值) - 右尾检验"),
              tags$li("或 H₁: μ < μ₀ (样本均值小于特定值) - 左尾检验")
            )
          ),
          tags$li(
            strong("独立样本t检验 (Independent Samples t-test):"),
            "用于比较两个独立样本的均值是否存在显著差异。例如，比较两种不同教学方法的效果。",
            tags$ul(
              tags$li("原假设 H₀: μ₁ = μ₂ (两组均值相等)"),
              tags$li("备择假设 H₁: μ₁ ≠ μ₂ (两组均值不相等) - 双尾检验"),
              tags$li("或 H₁: μ₁ > μ₂ (组1均值大于组2) - 右尾检验"),
              tags$li("或 H₁: μ₁ < μ₂ (组1均值小于组2) - 左尾检验")
            )
          ),
          tags$li(
            strong("配对样本t检验 (Paired Samples t-test):"),
            "用于比较同一组受试者在两种不同条件下的表现是否存在显著差异。例如，同一组学生在处理前后的测试分数。",
            tags$ul(
              tags$li("原假设 H₀: μd = 0 (配对差异的均值为零)"),
              tags$li("备择假设 H₁: μd ≠ 0 (配对差异的均值不为零) - 双尾检验"),
              tags$li("或 H₁: μd > 0 (配对差异的均值大于零) - 右尾检验"),
              tags$li("或 H₁: μd < 0 (配对差异的均值小于零) - 左尾检验")
            )
          )
        ),
        
        h4("t检验的基本假设"),
        tags$ul(
          tags$li("数据应来自正态分布，或在大样本情况下近似正态分布。"),
          tags$li("对于独立样本t检验，样本应该是独立的。"),
          tags$li("对于独立样本t检验，可以选择假设两组方差相等或不相等 (Welch's t-test)。"),
          tags$li("对于配对样本t检验，差异值应来自正态分布。")
        ),
        
        h4("如何解释t检验结果"),
        tags$ul(
          tags$li("p值 < α (通常为0.05): 拒绝原假设，认为差异具有统计学显著性。"),
          tags$li("p值 ≥ α: 不拒绝原假设，认为没有足够证据表明存在显著差异。"),
          tags$li("置信区间: 提供效应量的估计范围，不包含零表示差异显著。"),
          tags$li("效应量 (Cohen's d): 量化差异大小，约0.2为小效应，0.5为中等效应，0.8为大效应。")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "t检验练习",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("练习1: 选择合适的t检验类型"),
        p("以下场景分别应该使用哪种t检验？"),
        p("(a) 研究者想知道某品牌鞋子的平均使用寿命是否达到了宣称的500天。"),
        p("(b) 研究者比较两种不同减肥方法导致的体重下降量。参与者被随机分到两组，每组使用一种减肥方法。"),
        p("(c) 研究者测量了同一组学生在新教学方法实施前后的考试成绩。"),
        
        actionButton(ns("show_solution1"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution1 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("(a) 单样本t检验 - 因为研究者是将一个样本的平均值与一个已知值(500天)进行比较。"),
            p("(b) 独立样本t检验 - 因为有两个独立的组，每组使用不同的减肥方法。"),
            p("(c) 配对样本t检验 - 因为是同一组学生在前后两个时间点的测量，形成了自然的配对。")
          )
        ),
        
        hr(),
        
        h4("练习2: 解释检验结果"),
        p("某研究比较了两组学生在不同教学方法下的学习效果。研究者进行了独立样本t检验，结果为: t = 2.45, df = 38, p = 0.019, 95%CI = [0.72, 7.88], Cohen's d = 0.78。"),
        p("请解释这一结果的含义，并就教学方法的有效性给出建议。"),
        
        actionButton(ns("show_solution2"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution2 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("结果解释:"),
            tags$ul(
              tags$li("p值(0.019) < 0.05，表明结果具有统计学显著性，我们拒绝两组均值相等的原假设。"),
              tags$li("95%置信区间[0.72, 7.88]不包含0，支持两组存在实质性差异。"),
              tags$li("Cohen's d = 0.78接近0.8，表明这是一个接近大效应的中到大效应。"),
              tags$li("t值为正(2.45)，表明第一组的均值高于第二组。")
            ),
            p("建议:"),
            p("结果显示，比较的两种教学方法在学习效果上确实存在显著差异，且效应量较大。第一种教学方法的效果显著优于第二种方法。基于这一结果，建议教师可以优先考虑采用第一种教学方法，特别是当教学资源有限需要做出选择时。不过，在全面推广前，可能需要考虑其他因素，如成本、实施难度、适用性等，以及在不同背景和条件下的推广效果。")
          )
        ),
        
        hr(),
        
        h4("练习3: 分析实际数据"),
        p("使用\"减肥计划\"示例数据，进行配对样本t检验，分析减肥计划是否有效。"),
        p("(a) 明确原假设和备择假设，选择合适的备择假设方向。"),
        p("(b) 确定并运行合适的t检验类型。"),
        p("(c) 解释检验结果，包括p值、置信区间和效应量。"),
        
        actionButton(ns("show_solution3"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution3 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("(a) 假设设置:"),
            tags$ul(
              tags$li("原假设H₀: μd = 0 (减肥前后体重无显著差异)"),
              tags$li("备择假设H₁: μd < 0 (减肥后体重显著降低) - 左尾检验")
            ),
            p("选择左尾检验是因为我们预期减肥后体重会降低，即差异(前-后)为正值。"),
            p("(b) 检验类型:"),
            p("应选择配对样本t检验，因为数据是同一组人在减肥前后的测量，形成了自然的配对。"),
            p("(c) 结果解释:"),
            p("运行测试结果显示:"),
            p("t = 16.73, df = 9, p < 0.001"),
            p("95%置信区间: [3.01, 3.94]公斤"),
            p("Cohen's d = 5.29"),
            p("这意味着:"),
            tags$ul(
              tags$li("p值远小于0.05，我们可以拒绝原假设，得出减肥计划确实显著降低了参与者体重的结论。"),
              tags$li("置信区间完全大于0，表明减肥效果是实质性的，平均减重约3.0-3.9公斤。"),
              tags$li("Cohen's d = 5.29是一个极大的效应量，表明减肥计划的效果非常明显。"),
              tags$li("综合来看，减肥计划取得了统计学上显著且实际意义上重要的效果。")
            )
          )
        )
      )
    )
  )
}

# 服务器函数
tTestServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 获取数据
    test_data <- reactive({
      if (input$data_source == "example") {
        # 使用示例数据
        if (input$test_type == "one.sample") {
          # 单样本t检验
          if (input$example_dataset == "movie_ratings") {
            data1 <- sample_data$movie_ratings$action
            mu0 <- 4.0
          } else if (input$example_dataset == "student_scores") {
            data1 <- sample_data$student_scores$after
            mu0 <- 70
          } else if (input$example_dataset == "weight_loss") {
            data1 <- sample_data$weight_loss$after
            mu0 <- 85
          } else if (input$example_dataset == "treatment_effect") {
            data1 <- sample_data$treatment_effect$drug_a
            mu0 <- 15
          }
          list(data1 = data1, data2 = NULL, mu0 = mu0)
        } else if (input$test_type == "two.sample") {
          # 独立样本t检验
          if (input$example_dataset == "movie_ratings") {
            data1 <- sample_data$movie_ratings$action
            data2 <- sample_data$movie_ratings$comedy
          } else if (input$example_dataset == "student_scores") {
            data1 <- sample_data$student_scores$before
            data2 <- sample_data$student_scores$after
          } else if (input$example_dataset == "weight_loss") {
            data1 <- sample_data$weight_loss$before
            data2 <- sample_data$weight_loss$after
          } else if (input$example_dataset == "treatment_effect") {
            data1 <- sample_data$treatment_effect$drug_a
            data2 <- sample_data$treatment_effect$drug_b
          }
          list(data1 = data1, data2 = data2, mu0 = NULL)
        } else if (input$test_type == "paired") {
          # 配对样本t检验
          if (input$example_dataset == "movie_ratings") {
            # 创建假配对数据
            data1 <- sample_data$movie_ratings$action
            data2 <- sample_data$movie_ratings$comedy
          } else if (input$example_dataset == "student_scores") {
            data1 <- sample_data$student_scores$before
            data2 <- sample_data$student_scores$after
          } else if (input$example_dataset == "weight_loss") {
            data1 <- sample_data$weight_loss$before
            data2 <- sample_data$weight_loss$after
          } else if (input$example_dataset == "treatment_effect") {
            # 仅使用前几个数据点创建配对
            data1 <- sample_data$treatment_effect$drug_a[1:10]
            data2 <- sample_data$treatment_effect$drug_b[1:10]
          }
          list(data1 = data1, data2 = data2, mu0 = NULL)
        }
      } else {
        # 使用自定义数据
        if (input$test_type == "one.sample") {
          # 解析单样本数据
          data1 <- as.numeric(unlist(strsplit(gsub("[[:space:]]", "", input$custom_data1), ",")))
          mu0 <- input$null_value
          list(data1 = data1, data2 = NULL, mu0 = mu0)
        } else if (input$test_type == "two.sample") {
          # 解析独立样本数据
          data1 <- as.numeric(unlist(strsplit(gsub("[[:space:]]", "", input$custom_data1_two), ",")))
          data2 <- as.numeric(unlist(strsplit(gsub("[[:space:]]", "", input$custom_data2_two), ",")))
          list(data1 = data1, data2 = data2, mu0 = NULL)
        } else if (input$test_type == "paired") {
          # 解析配对样本数据
          data1 <- as.numeric(unlist(strsplit(gsub("[[:space:]]", "", input$custom_data1_paired), ",")))
          data2 <- as.numeric(unlist(strsplit(gsub("[[:space:]]", "", input$custom_data2_paired), ",")))
          list(data1 = data1, data2 = data2, mu0 = NULL)
        }
      }
    })
    
    # 运行t检验
    test_result <- eventReactive(input$run_test, {
      data <- test_data()
      
      if (input$test_type == "one.sample") {
        # 单样本t检验
        result <- run_one_sample_t_test(
          data = data$data1,
          mu = data$mu0,
          conf_level = input$conf_level,
          alternative = input$alternative
        )
      } else if (input$test_type == "two.sample") {
        # 独立样本t检验
        result <- run_two_sample_t_test(
          group1 = data$data1,
          group2 = data$data2,
          conf_level = input$conf_level,
          alternative = input$alternative,
          var_equal = if (input$data_source == "custom") input$var_equal else FALSE
        )
      } else if (input$test_type == "paired") {
        # 配对样本t检验
        result <- run_paired_t_test(
          group1 = data$data1,
          group2 = data$data2,
          conf_level = input$conf_level,
          alternative = input$alternative
        )
      }
      
      return(result)
    })
    
    # 显示数据摘要
    output$data_summary <- renderText({
      data <- test_data()
      
      if (input$test_type == "one.sample") {
        # 单样本数据摘要
        paste0(
          "样本量: ", length(data$data1), "\n",
          "平均值: ", round(mean(data$data1), 2), "\n",
          "标准差: ", round(sd(data$data1), 2), "\n",
          "最小值: ", round(min(data$data1), 2), "\n",
          "最大值: ", round(max(data$data1), 2), "\n",
          "原假设值 (μ₀): ", data$mu0
        )
      } else {
        # 双样本或配对样本数据摘要
        paste0(
          "组1样本量: ", length(data$data1), "\n",
          "组1平均值: ", round(mean(data$data1), 2), "\n",
          "组1标准差: ", round(sd(data$data1), 2), "\n\n",
          "组2样本量: ", length(data$data2), "\n",
          "组2平均值: ", round(mean(data$data2), 2), "\n",
          "组2标准差: ", round(sd(data$data2), 2), "\n\n",
          "均值差异 (组1 - 组2): ", round(mean(data$data1) - mean(data$data2), 2)
        )
      }
    })
    
    # 显示t检验结果
    output$test_result <- renderText({
      req(test_result())
      
      # 格式化t检验结果为可读文本
      format_t_test_result(test_result())
    })
    
    # 绘制t检验可视化图
    output$test_plot <- renderPlot({
      req(test_result())
      
      data <- test_data()
      
      # 绘制t检验结果可视化图
      plot_t_test(
        data1 = data$data1,
        data2 = data$data2,
        test_result = test_result(),
        test_type = input$test_type
      )
    })
    
    # 练习解答按钮
    observeEvent(input$show_solution1, {
      # 解答按钮的逻辑在UI中通过conditionalPanel实现
    })
    
    observeEvent(input$show_solution2, {
      # 解答按钮的逻辑在UI中通过conditionalPanel实现
    })
    
    observeEvent(input$show_solution3, {
      # 解答按钮的逻辑在UI中通过conditionalPanel实现
    })
  })
} 