# tidyr基础练习

library(learnr)
library(tidyverse)

# 练习1: pivot_longer函数
tidyr_exercise1 <- function() {
  quiz(
    question("以下哪个代码可以将宽格式数据转换为长格式数据？",
      answer("wide_data %>% pivot_longer(cols = c(语文, 数学, 英语), names_to = \"科目\", values_to = \"成绩\")", correct = TRUE),
      answer("wide_data %>% pivot_wider(cols = c(语文, 数学, 英语), names_to = \"科目\", values_to = \"成绩\")"),
      answer("wide_data %>% pivot_longer(names_from = \"科目\", values_from = \"成绩\")"),
      answer("wide_data %>% gather(科目, 成绩, 语文, 数学, 英语)", correct = TRUE),
      allow_retry = TRUE
    ),
    question("pivot_longer函数的主要作用是什么？",
      answer("将宽格式数据转换为长格式数据，使数据更符合tidy data原则", correct = TRUE),
      answer("将长格式数据转换为宽格式数据，使数据更易于阅读"),
      answer("将多列合并为一列"),
      answer("将一列拆分为多列"),
      allow_retry = TRUE
    )
  )
}

# 练习2: pivot_wider函数
tidyr_exercise2 <- function() {
  quiz(
    question("以下哪个代码可以将长格式数据转换为宽格式数据？",
      answer("long_data %>% pivot_wider(names_from = \"科目\", values_from = \"成绩\")", correct = TRUE),
      answer("long_data %>% pivot_longer(names_from = \"科目\", values_from = \"成绩\")"),
      answer("long_data %>% pivot_wider(cols = c(语文, 数学, 英语), names_to = \"科目\", values_to = \"成绩\")"),
      answer("long_data %>% spread(科目, 成绩)", correct = TRUE),
      allow_retry = TRUE
    ),
    question("pivot_wider函数的主要作用是什么？",
      answer("将长格式数据转换为宽格式数据，使数据更易于阅读和理解", correct = TRUE),
      answer("将宽格式数据转换为长格式数据，使数据更符合tidy data原则"),
      answer("将多列合并为一列"),
      answer("将一列拆分为多列"),
      allow_retry = TRUE
    )
  )
}

# 练习3: separate函数
tidyr_exercise3 <- function() {
  quiz(
    question("以下哪个代码可以将日期列拆分为年份、月份和日期三列？",
      answer("data %>% separate(col = 日期, into = c(\"年份\", \"月份\", \"日期\"), sep = \"-\")", correct = TRUE),
      answer("data %>% separate(col = c(\"年份\", \"月份\", \"日期\"), into = 日期, sep = \"-\")"),
      answer("data %>% unite(col = 日期, c(\"年份\", \"月份\", \"日期\"), sep = \"-\")"),
      answer("data %>% split(日期, c(\"年份\", \"月份\", \"日期\"), sep = \"-\")"),
      allow_retry = TRUE
    ),
    question("separate函数的主要作用是什么？",
      answer("将一列拆分为多列，通常基于某个分隔符", correct = TRUE),
      answer("将多列合并为一列，通常使用某个分隔符连接"),
      answer("将宽格式数据转换为长格式数据"),
      answer("将长格式数据转换为宽格式数据"),
      allow_retry = TRUE
    )
  )
}

# 练习4: unite函数
tidyr_exercise4 <- function() {
  quiz(
    question("以下哪个代码可以将姓和名两列合并为一个姓名列？",
      answer("data %>% unite(col = \"姓名\", c(姓, 名), sep = \"\")", correct = TRUE),
      answer("data %>% separate(col = \"姓名\", into = c(姓, 名), sep = \"\")"),
      answer("data %>% unite(col = c(姓, 名), into = \"姓名\", sep = \"\")"),
      answer("data %>% combine(col = \"姓名\", c(姓, 名), sep = \"\")"),
      allow_retry = TRUE
    ),
    question("unite函数的主要作用是什么？",
      answer("将多列合并为一列，通常使用某个分隔符连接", correct = TRUE),
      answer("将一列拆分为多列，通常基于某个分隔符"),
      answer("将宽格式数据转换为长格式数据"),
      answer("将长格式数据转换为宽格式数据"),
      allow_retry = TRUE
    )
  )
}

# 练习5: 编程练习
tidyr_exercise5 <- function() {
  exercise <- exercise_server("tidyr_code", 
    function(input, output, session) {
      # 检查代码是否正确
      check_code <- function(user_code) {
        # 创建示例数据
        wide_data <- data.frame(
          学生 = c("小明", "小红", "小李"),
          语文 = c(85, 92, 78),
          数学 = c(90, 88, 95),
          英语 = c(82, 90, 85)
        )
        
        # 尝试执行用户代码
        tryCatch({
          # 创建一个新的环境来执行代码
          env <- new.env()
          
          # 在环境中加载数据
          env$wide_data <- wide_data
          
          # 执行用户代码
          result <- eval(parse(text = user_code), env)
          
          # 检查结果是否符合要求
          if (!is.data.frame(result)) {
            return(list(success = FALSE, message = "结果应该是一个数据框。"))
          }
          
          if (!"学生" %in% names(result) || !"科目" %in% names(result) || !"成绩" %in% names(result)) {
            return(list(success = FALSE, message = "结果应该包含学生、科目和成绩列。"))
          }
          
          if (nrow(result) != nrow(wide_data) * (ncol(wide_data) - 1)) {
            return(list(success = FALSE, message = "结果行数不正确。应该有9行（3个学生 × 3个科目）。"))
          }
          
          return(list(success = TRUE, message = "恭喜！你的代码正确。"))
        }, error = function(e) {
          return(list(success = FALSE, message = paste("代码执行错误:", e$message)))
        })
      }
      
      # 设置检查按钮
      observeEvent(input$check, {
        result <- check_code(input$code)
        if (result$success) {
          showNotification(result$message, type = "message")
        } else {
          showNotification(result$message, type = "error")
        }
      })
    }
  )
  
  tagList(
    p("使用tidyr函数编写代码，将宽格式的学生成绩数据转换为长格式。"),
    p("示例数据："),
    verbatimTextOutput("wide_data_preview"),
    p("要求："),
    tags$ul(
      tags$li("使用pivot_longer函数"),
      tags$li("结果应包含学生、科目和成绩列"),
      tags$li("科目列应包含语文、数学、英语")
    ),
    textAreaInput("tidyr_code", "代码", rows = 10, 
      value = "# 在这里编写代码\nwide_data %>%\n  # 使用pivot_longer函数将宽格式转换为长格式\n  "),
    actionButton("check", "检查代码"),
    verbatimTextOutput("result")
  )
}

# 完整的tidyr练习
tidyr_exercises <- function() {
  tabsetPanel(
    tabPanel("练习1: pivot_longer函数", tidyr_exercise1()),
    tabPanel("练习2: pivot_wider函数", tidyr_exercise2()),
    tabPanel("练习3: separate函数", tidyr_exercise3()),
    tabPanel("练习4: unite函数", tidyr_exercise4()),
    tabPanel("练习5: 编程练习", tidyr_exercise5())
  )
} 