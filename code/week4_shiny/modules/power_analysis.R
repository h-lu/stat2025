# 检验功效分析模块
# 演示功效分析和样本量计算

# UI函数
powerAnalysisUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      create_title_card(
        title = "检验功效分析演示",
        subtitle = "探索检验功效与样本量、效应量和显著性水平的关系"
      )
    ),
    
    fluidRow(
      box(
        width = 4,
        title = "参数设置",
        status = "primary",
        solidHeader = TRUE,
        
        # 检验类型选择
        radioButtons(
          ns("test_type"),
          "检验类型:",
          choices = list(
            "单样本t检验" = "one.sample",
            "独立样本t检验" = "two.sample",
            "配对样本t检验" = "paired"
          ),
          selected = "two.sample"
        ),
        
        # 备择假设类型
        radioButtons(
          ns("alternative"),
          "备择假设类型:",
          choices = list(
            "双尾 (≠)" = "two.sided",
            "右尾 (>)" = "greater",
            "左尾 (<)" = "less"
          ),
          selected = "two.sided"
        ),
        
        # 效应量滑块
        sliderInput(
          ns("effect_size"),
          "效应量 (Cohen's d):",
          min = 0.1,
          max = 1.0,
          value = 0.5,
          step = 0.05
        ),
        
        # 显著性水平滑块
        sliderInput(
          ns("sig_level"),
          "显著性水平 (α):",
          min = 0.01,
          max = 0.10,
          value = 0.05,
          step = 0.01
        ),
        
        # 期望功效滑块
        sliderInput(
          ns("desired_power"),
          "期望功效 (1-β):",
          min = 0.7,
          max = 0.95,
          value = 0.8,
          step = 0.05
        ),
        
        # 更新图表按钮
        actionButton(
          ns("update_plots"),
          "更新图表",
          icon = icon("sync"),
          class = "btn-primary btn-block"
        )
      ),
      
      # 功效分析结果
      box(
        width = 8,
        title = "功效分析结果",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(
          column(
            width = 6,
            h4("所需样本量"),
            verbatimTextOutput(ns("sample_size_result")),
            tags$div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
              tags$p(
                style = "margin: 0;",
                "基于当前设置，达到期望功效所需的样本量"
              )
            )
          ),
          column(
            width = 6,
            h4("功效计算"),
            verbatimTextOutput(ns("power_result")),
            numericInput(
              ns("custom_sample_size"),
              "自定义样本量:",
              value = 30,
              min = 5,
              max = 1000,
              step = 5
            )
          )
        )
      )
    ),
    
    # 可视化图表
    fluidRow(
      tabBox(
        width = 12,
        title = "功效分析可视化",
        id = ns("power_plots"),
        
        tabPanel(
          "功效曲线",
          plotOutput(ns("power_curve_plot"), height = "400px")
        ),
        tabPanel(
          "样本量曲线",
          plotOutput(ns("sample_size_plot"), height = "400px")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "检验功效分析概念解释",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("什么是检验功效?"),
        p("检验功效（Power）是当备择假设为真时，正确拒绝原假设的概率。换句话说，是成功检测到真实存在的效应的概率。功效等于1-β，其中β是Type II错误的概率。"),
        
        h4("为什么功效分析很重要?"),
        p("在研究设计阶段，功效分析有助于确定所需的样本量，以确保有足够的统计能力检测感兴趣的效应。功效低的研究可能会因样本量不足而无法检测到真实效应，导致错误的否定结论。"),
        
        h4("影响检验功效的因素"),
        tags$ul(
          tags$li(strong("样本量 (n):"), "样本量越大，功效越高。这是因为大样本能减小抽样误差，提供更准确的估计。"),
          tags$li(strong("效应量 (Effect Size):"), "效应量衡量真实效应的大小。效应量越大，功效越高，因为较大的效应更容易被检测到。"),
          tags$li(strong("显著性水平 (α):"), "较高的显著性水平（如α=0.10而非0.05）会提高功效，因为设置了更宽松的拒绝H₀的标准，但同时增加了Type I错误的风险。"),
          tags$li(strong("检验方向:"), "单尾检验比双尾检验具有更高的功效（当效应方向与检验方向一致时），因为所有的α都分配在一个方向上。")
        ),
        
        h4("常见的效应量解释"),
        tags$table(
          class = "table table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Cohen's d"),
              tags$th("效应大小解释")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("0.2"),
              tags$td("小效应")
            ),
            tags$tr(
              tags$td("0.5"),
              tags$td("中等效应")
            ),
            tags$tr(
              tags$td("0.8"),
              tags$td("大效应")
            )
          )
        ),
        
        h4("功效分析在研究中的应用"),
        tags$ol(
          tags$li("在研究设计阶段，确定适当的样本量，以达到期望的功效（通常为0.8或0.9）。"),
          tags$li("评估已发表研究的功效，判断其结论的可靠性。"),
          tags$li("当结果不显著时，区分真正的\"无效应\"和\"统计功效不足\"的情况。"),
          tags$li("在研究资源有限的情况下，帮助研究者做出样本量和检验力的权衡决策。")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "检验功效分析练习",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("练习1: 样本量计算"),
        p("一项研究旨在检验某种训练方法对记忆力的影响。研究者希望能够检测到中等大小的效应（Cohen's d = 0.5），并希望达到80%的检验功效，同时使用0.05的显著性水平进行双尾检验。"),
        p("研究者计划使用独立样本t检验比较实验组和对照组。每组需要多少受试者？"),
        
        actionButton(ns("show_solution1"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution1 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("使用pwr.t.test函数计算:"),
            tags$pre(
              "pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.8, type = 'two.sample', alternative = 'two.sided')"
            ),
            p("计算结果: 每组需要约64个受试者，总共需要128个受试者。"),
            p("解释: 为了在独立样本设计中，使用显著性水平0.05的双尾检验，有80%的把握检测到0.5的效应量，研究者需要在每组招募64名受试者。如果样本量小于这个数字，研究可能缺乏足够的统计功效来检测中等大小的效应。")
          )
        ),
        
        hr(),
        
        h4("练习2: 不同检验类型的功效比较"),
        p("假设研究者在测量某种处理前后的变化，预期效应量为Cohen's d = 0.4。"),
        p("如果研究者有30名受试者，分别使用以下设计的检验功效有多大？"),
        p("(a) 单样本t检验（与已知均值比较）"),
        p("(b) 独立样本t检验（两组各15人）"),
        p("(c) 配对样本t检验（处理前后比较）"),
        
        actionButton(ns("show_solution2"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution2 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("对于每种检验类型，我们使用pwr.t.test计算功效:"),
            tags$pre(
              "(a) 单样本t检验: pwr.t.test(d = 0.4, n = 30, sig.level = 0.05, type = 'one.sample', alternative = 'two.sided')\n",
              "功效 ≈ 0.68 (68%)\n\n",
              "(b) 独立样本t检验: pwr.t.test(d = 0.4, n = 15, sig.level = 0.05, type = 'two.sample', alternative = 'two.sided')\n",
              "功效 ≈ 0.31 (31%)\n\n",
              "(c) 配对样本t检验: pwr.t.test(d = 0.4, n = 30, sig.level = 0.05, type = 'paired', alternative = 'two.sided')\n",
              "功效 ≈ 0.68 (68%)"
            ),
            p("结论：在相同效应量和总样本量的情况下，独立样本t检验的功效（31%）明显低于单样本和配对样本t检验（68%）。这是因为独立样本设计中样本被分成两组，每组只有15人，而且组间比较通常有更大的变异性。如果可能，对于前后测设计，配对样本t检验通常是更有效的选择。")
          )
        ),
        
        hr(),
        
        h4("练习3: 功效和效应量的权衡"),
        p("某研究者计划在一个小规模研究中最多招募40名参与者。如果使用独立样本t检验（分为两组），显著性水平为0.05，那么研究者能够以80%的功效检测到的最小效应量是多少？"),
        
        actionButton(ns("show_solution3"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution3 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("我们需要解出效应量d，使得给定条件下功效为0.8:"),
            tags$pre(
              "pwr.t.test(n = 20, sig.level = 0.05, power = 0.8, type = 'two.sample', alternative = 'two.sided')"
            ),
            p("计算结果: 最小可检测效应量 d ≈ 0.91"),
            p("解释: 使用总共40名参与者（每组20名）进行独立样本t检验，研究者只能以80%的功效检测到0.91或更大的效应量。按照Cohen的标准，这已经是一个大效应。如果预期中的实际效应较小，这个样本量将不足以可靠地检测到它。"),
            p("研究者有几个选择:"),
            tags$ul(
              tags$li("增加样本量（如果可能）"),
              tags$li("考虑使用配对设计来提高功效"),
              tags$li("降低期望功效（例如接受70%而非80%）"),
              tags$li("将研究定位为探索性研究，承认其功效限制")
            )
          )
        )
      )
    )
  )
}

# 服务器函数
powerAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 计算所需样本量
    required_sample_size <- reactive({
      # 使用pwr包计算样本量
      sample_size <- calculate_sample_size(
        d = input$effect_size,
        sig_level = input$sig_level,
        power = input$desired_power,
        type = input$test_type,
        alternative = input$alternative
      )
      
      return(sample_size)
    })
    
    # 计算自定义样本量的功效
    custom_power <- reactive({
      # 使用pwr包计算功效
      power <- calculate_power(
        d = input$effect_size,
        sig_level = input$sig_level,
        n = input$custom_sample_size,
        type = input$test_type,
        alternative = input$alternative
      )
      
      return(power)
    })
    
    # 显示样本量计算结果
    output$sample_size_result <- renderText({
      sample_size <- required_sample_size()
      
      if (is.na(sample_size)) {
        return("计算出错。请调整参数后重试。")
      }
      
      test_description <- case_when(
        input$test_type == "one.sample" ~ "单样本t检验",
        input$test_type == "two.sample" ~ paste0("独立样本t检验 (每组 ", ceiling(sample_size/2), " 人)"),
        input$test_type == "paired" ~ "配对样本t检验",
        TRUE ~ input$test_type
      )
      
      paste0(
        "检验类型: ", test_description, "\n",
        "效应量 (d): ", input$effect_size, "\n",
        "显著性水平 (α): ", input$sig_level, "\n",
        "期望功效 (1-β): ", input$desired_power, "\n",
        "所需样本量: ", sample_size, " 人\n"
      )
    })
    
    # 显示功效计算结果
    output$power_result <- renderText({
      power <- custom_power()
      
      if (is.na(power)) {
        return("计算出错。请调整参数后重试。")
      }
      
      test_description <- case_when(
        input$test_type == "one.sample" ~ "单样本t检验",
        input$test_type == "two.sample" ~ paste0("独立样本t检验 (每组 ", ceiling(input$custom_sample_size/2), " 人)"),
        input$test_type == "paired" ~ "配对样本t检验",
        TRUE ~ input$test_type
      )
      
      paste0(
        "检验类型: ", test_description, "\n",
        "效应量 (d): ", input$effect_size, "\n",
        "显著性水平 (α): ", input$sig_level, "\n",
        "样本量: ", input$custom_sample_size, " 人\n",
        "计算得功效: ", round(power * 100, 1), "%\n"
      )
    })
    
    # 绘制功效曲线
    output$power_curve_plot <- renderPlot({
      req(input$update_plots)
      
      plot_power_analysis(
        effect_size_range = seq(0.1, 1, by = 0.1),
        sig_level = input$sig_level,
        sample_sizes = c(10, 20, 30, 50, 100),
        type = input$test_type,
        alternative = input$alternative
      )
    })
    
    # 绘制样本量曲线
    output$sample_size_plot <- renderPlot({
      req(input$update_plots)
      
      plot_sample_size_calculation(
        effect_size_range = seq(0.1, 1, by = 0.1),
        sig_level = input$sig_level,
        power_levels = c(0.8, 0.9, 0.95),
        type = input$test_type,
        alternative = input$alternative
      )
    })
    
    # 更新图表按钮
    observeEvent(input$update_plots, {
      # 图表在renderPlot时会自动更新
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