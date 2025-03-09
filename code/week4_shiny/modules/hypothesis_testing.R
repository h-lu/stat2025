# 假设检验错误类型模块
# 演示Type I错误、Type II错误和检验功效的概念

# UI函数
hypothesisTestingErrorsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      create_title_card(
        title = "假设检验错误类型演示",
        subtitle = "理解Type I错误(α)、Type II错误(β)和检验功效(1-β)的概念及其关系"
      )
    ),
    
    fluidRow(
      box(
        width = 4,
        title = "参数设置",
        status = "primary",
        solidHeader = TRUE,
        
        # 原假设均值滑块
        sliderInput(
          ns("mu0"),
          "H₀ 总体均值 (μ₀):",
          min = 50,
          max = 150,
          value = 100,
          step = 5
        ),
        
        # 备择假设均值滑块
        sliderInput(
          ns("mu1"),
          "H₁ 总体均值 (μ₁):",
          min = 50,
          max = 150,
          value = 110,
          step = 5
        ),
        
        # 总体标准差滑块
        sliderInput(
          ns("sigma"),
          "总体标准差 (σ):",
          min = 5,
          max = 30,
          value = 15,
          step = 1
        ),
        
        # 显著性水平滑块
        sliderInput(
          ns("alpha"),
          "显著性水平 (α):",
          min = 0.01,
          max = 0.20,
          value = 0.05,
          step = 0.01
        ),
        
        # 更新图表按钮
        actionButton(
          ns("update_plot"),
          "更新图表",
          icon = icon("sync"),
          class = "btn-primary btn-block"
        )
      ),
      
      # 错误类型可视化
      box(
        width = 8,
        title = "假设检验错误类型可视化",
        status = "primary",
        solidHeader = TRUE,
        
        plotOutput(ns("errors_plot"), height = "300px"),
        
        div(
          style = "margin-top: 20px;",
          fluidRow(
            column(
              width = 4,
              create_stat_card(
                title = "Type I 错误 (α)",
                value = textOutput(ns("type1_error")),
                color = app_colors$danger
              )
            ),
            column(
              width = 4,
              create_stat_card(
                title = "Type II 错误 (β)",
                value = textOutput(ns("type2_error")),
                color = app_colors$warning
              )
            ),
            column(
              width = 4,
              create_stat_card(
                title = "检验功效 (1-β)",
                value = textOutput(ns("power")),
                color = app_colors$success
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "假设检验错误概念解释",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("假设检验的基本概念"),
        p("假设检验是一种统计推断方法，用于判断样本数据是否为特定假设提供了足够的证据。在假设检验中，我们通常设定两个假设："),
        tags$ul(
          tags$li(strong("原假设 (H₀):"), "通常是我们想要检验的陈述，比如\"没有效应\"或\"没有差异\"。"),
          tags$li(strong("备择假设 (H₁):"), "与原假设相反的陈述，通常是研究者希望证明的。")
        ),
        
        h4("两类错误"),
        tags$ul(
          tags$li(strong("Type I 错误 (α, 拒真错误):"), "当原假设实际上是真的，但被我们错误地拒绝了。这个错误的概率就是显著性水平α。"),
          tags$li(strong("Type II 错误 (β, 存伪错误):"), "当原假设实际上是假的，但我们没有拒绝它。")
        ),
        
        h4("检验功效 (Power)"),
        p("检验功效是正确拒绝错误的原假设的概率，等于1-β。它表示当备择假设为真时，我们能够通过检验正确地拒绝原假设的能力。"),
        
        h4("这些概念之间的关系"),
        p("在本演示中，我们假设有两个正态分布总体：一个代表H₀为真的情况（均值为μ₀），另一个代表H₁为真的情况（均值为μ₁）。"),
        tags$ul(
          tags$li("如果我们从H₀总体抽样，任何落在拒绝区域（临界值右侧）的样本均值都会导致Type I错误。"),
          tags$li("如果我们从H₁总体抽样，任何落在接受区域（临界值左侧）的样本均值都会导致Type II错误。"),
          tags$li("检验功效是从H₁总体抽样时，样本均值落在拒绝区域的概率。")
        ),
        
        h4("影响错误率和功效的因素"),
        tags$ul(
          tags$li(strong("显著性水平 (α):"), "增加α会减少β，提高功效，但也增加了Type I错误的风险。"),
          tags$li(strong("效应量 (|μ₁ - μ₀|):"), "两个总体均值差异越大，越容易检测到差异，功效越高。"),
          tags$li(strong("样本量 (n):"), "增加样本量会减小标准误差，提高功效。本演示中未包含此参数。"),
          tags$li(strong("总体标准差 (σ):"), "标准差越小，数据变异性越小，功效越高。")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "假设检验错误练习",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        h4("练习1: 理解Type I和Type II错误"),
        p("在一项研究中，研究者假设某种新药可以降低患者的血压。她设置了以下假设："),
        p("H₀: 新药无效（μ = 0，平均血压降低量为零）"),
        p("H₁: 新药有效（μ > 0，平均血压有显著降低）"),
        p("请说明在这个情境中，Type I错误和Type II错误各代表什么？哪种错误更严重？"),
        
        actionButton(ns("show_solution1"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution1 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            tags$ul(
              tags$li(strong("Type I错误："), "新药实际上无效（H₀为真），但研究结果错误地表明它有效（拒绝H₀）。这相当于批准了一种实际无效的药物。"),
              tags$li(strong("Type II错误："), "新药实际上有效（H₁为真），但研究结果未能检测到其效果（未拒绝H₀）。这相当于错过了一种有效的药物。")
            ),
            p("哪种错误更严重取决于具体情境和后果。在药物研究中，Type I错误可能导致批准无效甚至有害的药物，这通常被认为更严重，因此α通常设置得较小（如0.05或0.01）。但如果错过一种可能挽救生命的药物，Type II错误也可能有严重后果。")
          )
        ),
        
        hr(),
        
        h4("练习2: 提高检验功效的方法"),
        p("研究者发现他的研究功效太低（如0.6）。请列举至少三种可以提高研究功效的方法，并解释原因。"),
        
        actionButton(ns("show_solution2"), "查看解答", class = "btn-info"),
        
        conditionalPanel(
          condition = "input.show_solution2 % 2 == 1",
          ns = ns,
          div(
            class = "alert alert-success",
            style = "margin-top: 10px;",
            h4("解答:"),
            p("提高研究功效的方法："),
            tags$ol(
              tags$li(strong("增加样本量："), "更大的样本会减小标准误差，使得检测到真实效应的可能性更大。这是提高功效最直接的方法。"),
              tags$li(strong("增加显著性水平α："), "如从0.05增加到0.10，会降低拒绝H₀的标准，提高功效，但同时也增加了Type I错误的风险。"),
              tags$li(strong("减小数据变异性："), "通过改进测量方法、控制外部变量或使用更均质的样本来减小标准差。"),
              tags$li(strong("增加效应量："), "如可能的话，调整实验设计以增强处理效果，使得效应更容易被检测到。"),
              tags$li(strong("使用单尾检验："), "如果理论上效应方向是已知的，用单尾检验替代双尾检验可以提高功效。"),
              tags$li(strong("使用配对设计："), "对于前后测设计，使用配对样本可以减少个体间差异，提高功效。"),
              tags$li(strong("使用协变量分析："), "纳入影响因变量的协变量可以减少剩余变异，提高功效。")
            ),
            p("研究者应根据具体研究情况，选择合适的方法来提高功效，同时权衡成本和伦理因素。")
          )
        )
      )
    )
  )
}

# 服务器函数
hypothesisTestingErrorsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 计算错误率和功效
    errors_result <- reactive({
      # 计算临界值
      critical_value <- input$mu0 + qnorm(1 - input$alpha) * input$sigma
      
      # 计算Type I错误率 (α)
      type1_error <- 1 - pnorm(critical_value, mean = input$mu0, sd = input$sigma)
      
      # 计算Type II错误率 (β)
      type2_error <- pnorm(critical_value, mean = input$mu1, sd = input$sigma)
      
      # 计算检验功效 (1-β)
      power <- 1 - type2_error
      
      list(
        mu0 = input$mu0,
        mu1 = input$mu1,
        sigma = input$sigma,
        alpha = input$alpha,
        critical_value = critical_value,
        type1_error = type1_error,
        type2_error = type2_error,
        power = power
      )
    })
    
    # 绘制错误类型可视化图
    output$errors_plot <- renderPlot({
      req(errors_result())
      plot_hypothesis_testing_errors(
        mu0 = errors_result()$mu0,
        mu1 = errors_result()$mu1,
        sigma = errors_result()$sigma,
        alpha = errors_result()$alpha
      )
    })
    
    # 显示错误率和功效
    output$type1_error <- renderText({
      paste0(round(errors_result()$type1_error * 100, 2), "%")
    })
    
    output$type2_error <- renderText({
      paste0(round(errors_result()$type2_error * 100, 2), "%")
    })
    
    output$power <- renderText({
      paste0(round(errors_result()$power * 100, 2), "%")
    })
    
    # 更新图表按钮
    observeEvent(input$update_plot, {
      # 触发重新计算和绘图
      errors_result()
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