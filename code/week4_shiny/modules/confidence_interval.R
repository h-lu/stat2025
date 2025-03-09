# 置信区间模块
# 演示置信区间的概念和影响因素

# UI函数
confidenceIntervalUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      create_title_card(
        title = "置信区间演示",
        subtitle = "探索影响置信区间宽度的三个主要因素：样本量、置信水平和总体标准差"
      )
    ),
    
    fluidRow(
      box(
        width = 4,
        title = "参数设置",
        status = "primary",
        solidHeader = TRUE,
        
        # 样本量滑块
        sliderInput(
          ns("sample_size"),
          "样本量 (n):",
          min = 5,
          max = 100,
          value = 30,
          step = 5
        ),
        
        # 总体均值滑块
        sliderInput(
          ns("population_mean"),
          "总体均值 (μ):",
          min = 0,
          max = 100,
          value = 50,
          step = 5
        ),
        
        # 总体标准差滑块
        sliderInput(
          ns("population_sd"),
          "总体标准差 (σ):",
          min = 1,
          max = 30,
          value = 15,
          step = 1
        ),
        
        # 置信水平滑块
        sliderInput(
          ns("confidence_level"),
          "置信水平 (1-α):",
          min = 0.8,
          max = 0.99,
          value = 0.95,
          step = 0.01
        ),
        
        # 是否已知总体标准差
        checkboxInput(
          ns("is_known_sigma"),
          "已知总体标准差",
          value = FALSE
        ),
        
        # 随机生成样本按钮
        actionButton(
          ns("generate_sample"),
          "生成新样本",
          icon = icon("random"),
          class = "btn-primary btn-block"
        )
      ),
      
      # 置信区间可视化
      box(
        width = 8,
        title = "置信区间可视化",
        status = "primary",
        solidHeader = TRUE,
        
        plotOutput(ns("ci_plot"), height = "300px"),
        
        div(
          style = "margin-top: 20px;",
          h4("置信区间详细信息:"),
          verbatimTextOutput(ns("ci_details"))
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "置信区间概念解释",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("什么是置信区间?"),
        p("置信区间是一个统计概念，用于表示估计的总体参数的可能范围。它提供了一个区间估计，而不仅仅是一个点估计。"),
        
        h4("置信水平 (1-α) 的含义"),
        p("置信水平表示重复抽样多次时，置信区间包含总体参数真值的概率。例如，95%的置信水平意味着，如果从同一总体中重复抽取样本并计算置信区间，那么约有95%的置信区间会包含总体参数的真实值。"),
        
        h4("影响置信区间宽度的因素"),
        tags$ul(
          tags$li(strong("样本量 (n):"), "样本量越大，置信区间越窄。样本量增加会减小标准误差。"),
          tags$li(strong("置信水平 (1-α):"), "置信水平越高，置信区间越宽。要获得更高的确定性，区间需要更宽。"),
          tags$li(strong("总体标准差 (σ):"), "标准差越大，置信区间越宽。数据变异性增加会导致估计的不确定性增加。")
        ),
        
        h4("置信区间的计算公式"),
        withMathJax(),
        helpText("未知总体标准差时（使用t分布）:"),
        helpText("$$\\bar{x} \\pm t_{\\alpha/2, n-1} \\frac{s}{\\sqrt{n}}$$"),
        helpText("已知总体标准差时（使用Z分布）:"),
        helpText("$$\\bar{x} \\pm Z_{\\alpha/2} \\frac{\\sigma}{\\sqrt{n}}$$"),
        helpText("其中 \\(\\bar{x}\\) 是样本均值, \\(s\\) 是样本标准差, \\(\\sigma\\) 是总体标准差, \\(n\\) 是样本量, \\(t_{\\alpha/2, n-1}\\) 是自由度为 n-1 的t分布的临界值, \\(Z_{\\alpha/2}\\) 是标准正态分布的临界值。")
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "置信区间练习",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("练习1: 手动计算置信区间"),
        p("某研究者测量了一组25名学生的反应时间（毫秒），得到样本均值为350毫秒，样本标准差为40毫秒。"),
        p("计算95%的置信区间。"),
        
        actionButton(ns("show_solution1"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution1 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("解答步骤:"),
            tags$ol(
              tags$li("确定样本统计量: 样本均值 = 350, 样本标准差 = 40, 样本量 = 25"),
              tags$li("确定临界值: 对于95%置信水平和自由度为24的t分布, t(0.025, 24) = 2.064"),
              tags$li("计算标准误差: SE = s/√n = 40/√25 = 40/5 = 8"),
              tags$li("计算边际误差: ME = t × SE = 2.064 × 8 = 16.51"),
              tags$li("计算置信区间: 样本均值 ± ME = 350 ± 16.51 = [333.49, 366.51]")
            ),
            p("因此, 95%置信区间为 [333.49, 366.51] 毫秒")
          )
        ),
        
        hr(),
        
        h4("练习2: 理解不同置信水平"),
        p("如果在上面的例子中，研究者希望使用99%的置信水平而不是95%，置信区间会如何变化？为什么？"),
        
        actionButton(ns("show_solution2"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution2 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("对于99%置信水平, t(0.005, 24) = 2.797"),
            p("新的边际误差: ME = 2.797 × 8 = 22.38"),
            p("99%置信区间: 350 ± 22.38 = [327.62, 372.38]"),
            p("观察: 99%置信区间比95%置信区间更宽。这是因为更高的置信水平需要更大的范围才能有更高的确定性包含总体参数真值。置信水平越高，临界值越大，从而边际误差越大，置信区间越宽。")
          )
        )
      )
    )
  )
}

# 服务器函数
confidenceIntervalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 生成样本数据
    sample_data <- reactiveVal(NULL)
    
    # 计算置信区间
    ci_result <- reactive({
      # 检查样本数据是否存在
      if (is.null(sample_data())) {
        # 初次加载或点击按钮时生成数据
        generate_new_sample()
      }
      
      # 计算置信区间
      calculate_confidence_interval(
        data = sample_data(),
        conf_level = input$confidence_level,
        is_known_sigma = input$is_known_sigma,
        population_sigma = if (input$is_known_sigma) input$population_sd else NULL
      )
    })
    
    # 生成新样本的函数
    generate_new_sample <- function() {
      # 使用正态分布生成随机样本
      new_data <- rnorm(
        n = input$sample_size,
        mean = input$population_mean,
        sd = input$population_sd
      )
      
      sample_data(new_data)
    }
    
    # 响应生成新样本按钮
    observeEvent(input$generate_sample, {
      generate_new_sample()
    })
    
    # 绘制置信区间可视化图
    output$ci_plot <- renderPlot({
      req(sample_data())
      plot_confidence_interval(sample_data(), ci_result())
    })
    
    # 显示置信区间详细信息
    output$ci_details <- renderText({
      req(ci_result())
      
      result <- ci_result()
      
      paste0(
        "样本均值: ", round(result$sample_mean, 2), "\n",
        "样本标准差: ", round(result$sample_sd, 2), "\n",
        "样本量: ", result$sample_size, "\n",
        "置信水平: ", result$confidence_level * 100, "%\n",
        "使用分布: ", result$distribution, "\n",
        "临界值: ", round(result$critical_value, 3), "\n",
        "标准误差: ", round(result$standard_error, 3), "\n",
        "边际误差: ", round(result$margin_of_error, 3), "\n",
        result$confidence_level * 100, "%置信区间: [", 
        round(result$lower_bound, 2), ", ", 
        round(result$upper_bound, 2), "]"
      )
    })
    
    # 练习解答按钮
    observeEvent(input$show_solution1, {
      # 解答按钮的逻辑在UI中通过conditionalPanel实现
    })
    
    observeEvent(input$show_solution2, {
      # 解答按钮的逻辑在UI中通过conditionalPanel实现
    })
  })
} 