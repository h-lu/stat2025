# 基础练习模块
# 包含推断统计的基础练习题

# UI函数
basicExercisesUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      create_title_card(
        title = "基础练习",
        subtitle = "巩固推断统计的基本概念和方法"
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "练习说明",
        status = "info",
        solidHeader = TRUE,
        p("本模块包含一系列基础练习题，旨在帮助您巩固推断统计的核心概念。每道题目都配有详细解答和提示。"),
        p("建议先尝试独立思考和解答，必要时再查看提示或解答。"),
        p("练习内容涵盖：置信区间计算、假设检验设置、p值解释、检验功效分析和t检验类型选择等。")
      )
    ),
    
    # 练习题卡片
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("exercise_cards"))
      )
    )
  )
}

# 服务器函数
basicExercisesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 初始化显示状态
    show_solutions <- reactiveVal(rep(FALSE, length(basic_exercises)))
    show_hints <- reactiveVal(rep(FALSE, length(basic_exercises)))
    
    # 显示练习题卡片
    output$exercise_cards <- renderUI({
      ns <- session$ns
      
      exercise_ui_list <- lapply(seq_along(basic_exercises), function(i) {
        exercise <- basic_exercises[[i]]
        
        box(
          width = 12,
          title = paste0("练习 ", i, ": ", exercise$title),
          status = "primary",
          solidHeader = TRUE,
          
          div(
            p(exercise$description),
            
            if (!is.null(exercise$data)) {
              div(
                h5("数据："),
                p(paste0(
                  "样本量: ", exercise$data$n, ", ",
                  "样本均值: ", exercise$data$mean, ", ",
                  "样本标准差: ", exercise$data$sd, ", ",
                  "置信水平: ", exercise$data$conf_level * 100, "%"
                ))
              )
            },
            
            if (show_hints()[[i]]) {
              div(
                class = "alert alert-info",
                icon("lightbulb"), strong("提示："), 
                p(exercise$hints)
              )
            },
            
            if (show_solutions()[[i]]) {
              div(
                class = "alert alert-success",
                icon("check-circle"), strong("解答："), 
                p(exercise$solution)
              )
            },
            
            div(
              style = "margin-top: 10px;",
              actionButton(
                ns(paste0("btn_solution_", i)), 
                if (show_solutions()[[i]]) "隐藏答案" else "显示答案", 
                icon = if (show_solutions()[[i]]) icon("eye-slash") else icon("eye"),
                class = "btn-primary"
              ),
              actionButton(
                ns(paste0("btn_hint_", i)), 
                if (show_hints()[[i]]) "隐藏提示" else "显示提示", 
                icon = if (show_hints()[[i]]) icon("eye-slash") else icon("lightbulb"),
                class = "btn-info"
              )
            )
          )
        )
      })
      
      do.call(tagList, exercise_ui_list)
    })
    
    # 处理显示/隐藏解答按钮
    observe({
      lapply(seq_along(basic_exercises), function(i) {
        btn_id <- paste0("btn_solution_", i)
        
        observeEvent(input[[btn_id]], {
          current_solutions <- show_solutions()
          current_solutions[i] <- !current_solutions[i]
          show_solutions(current_solutions)
        })
      })
    })
    
    # 处理显示/隐藏提示按钮
    observe({
      lapply(seq_along(basic_exercises), function(i) {
        btn_id <- paste0("btn_hint_", i)
        
        observeEvent(input[[btn_id]], {
          current_hints <- show_hints()
          current_hints[i] <- !current_hints[i]
          show_hints(current_hints)
        })
      })
    })
  })
} 