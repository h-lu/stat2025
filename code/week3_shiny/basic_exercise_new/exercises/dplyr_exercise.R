# dplyr基础练习

library(learnr)
library(tidyverse)
library(palmerpenguins)

# 练习1: mutate函数
dplyr_exercise1 <- function() {
  quiz(
    question("以下哪个代码可以创建一个新列，计算企鹅的体重与嘴峰长度的比值？",
      answer("penguins %>% mutate(ratio = body_mass_g / bill_length_mm)", correct = TRUE),
      answer("penguins %>% select(ratio = body_mass_g / bill_length_mm)"),
      answer("penguins %>% summarise(ratio = body_mass_g / bill_length_mm)"),
      answer("penguins %>% filter(ratio = body_mass_g / bill_length_mm)"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以将企鹅的体重从克转换为千克？",
      answer("penguins %>% mutate(body_mass_kg = body_mass_g / 1000)", correct = TRUE),
      answer("penguins %>% mutate(body_mass_kg = body_mass_g * 1000)"),
      answer("penguins %>% select(body_mass_kg = body_mass_g / 1000)"),
      answer("penguins %>% summarise(body_mass_kg = body_mass_g / 1000)"),
      allow_retry = TRUE
    )
  )
}

# 练习2: arrange函数
dplyr_exercise2 <- function() {
  quiz(
    question("以下哪个代码可以按企鹅体重降序排序？",
      answer("penguins %>% arrange(desc(body_mass_g))", correct = TRUE),
      answer("penguins %>% arrange(body_mass_g)"),
      answer("penguins %>% order(desc(body_mass_g))"),
      answer("penguins %>% sort(desc(body_mass_g))"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以先按企鹅种类升序排序，再按体重降序排序？",
      answer("penguins %>% arrange(species, desc(body_mass_g))", correct = TRUE),
      answer("penguins %>% arrange(desc(species), desc(body_mass_g))"),
      answer("penguins %>% arrange(species, body_mass_g)"),
      answer("penguins %>% arrange(desc(species), body_mass_g)"),
      allow_retry = TRUE
    )
  )
}

# 练习3: group_by和summarise函数
dplyr_exercise3 <- function() {
  quiz(
    question("以下哪个代码可以计算每种企鹅的平均体重？",
      answer("penguins %>% group_by(species) %>% summarise(mean_weight = mean(body_mass_g, na.rm = TRUE))", correct = TRUE),
      answer("penguins %>% summarise(mean_weight = mean(body_mass_g, na.rm = TRUE)) %>% group_by(species)"),
      answer("penguins %>% group_by(species) %>% mean(body_mass_g, na.rm = TRUE)"),
      answer("penguins %>% filter(species) %>% summarise(mean_weight = mean(body_mass_g, na.rm = TRUE))"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以计算每个岛屿上每种企鹅的数量？",
      answer("penguins %>% group_by(island, species) %>% summarise(count = n())", correct = TRUE),
      answer("penguins %>% group_by(island, species) %>% count()"),
      answer("penguins %>% summarise(count = n()) %>% group_by(island, species)"),
      answer("penguins %>% count(island, species)", correct = TRUE),
      allow_retry = TRUE
    )
  )
}

# 练习4: 编程练习
dplyr_exercise4 <- function() {
  exercise <- exercise_server("dplyr_code", 
    function(input, output, session) {
      # 检查代码是否正确
      check_code <- function(user_code) {
        # 尝试执行用户代码
        tryCatch({
          # 创建一个新的环境来执行代码
          env <- new.env()
          
          # 在环境中加载数据
          env$penguins <- penguins
          
          # 执行用户代码
          result <- eval(parse(text = user_code), env)
          
          # 检查结果是否符合要求
          if (!is.data.frame(result)) {
            return(list(success = FALSE, message = "结果应该是一个数据框。"))
          }
          
          if (!"species" %in% names(result) || !"sex" %in% names(result) || 
              !"mean_body_mass" %in% names(result) || !"mean_bill_length" %in% names(result)) {
            return(list(success = FALSE, message = "结果应该包含species, sex, mean_body_mass和mean_bill_length列。"))
          }
          
          # 检查是否按species和sex分组
          if (nrow(result) != length(unique(paste(penguins$species, penguins$sex)))) {
            return(list(success = FALSE, message = "结果应该按species和sex分组。"))
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
    p("使用dplyr函数编写代码，计算每种企鹅每个性别的平均体重和平均嘴峰长度。"),
    p("要求："),
    tags$ul(
      tags$li("使用group_by和summarise函数"),
      tags$li("结果应包含species, sex, mean_body_mass和mean_bill_length列"),
      tags$li("处理缺失值"),
      tags$li("按species和sex排序")
    ),
    textAreaInput("dplyr_code", "代码", rows = 10, 
      value = "# 在这里编写代码\npenguins %>%\n  # 按species和sex分组\n  \n  # 计算平均值\n  \n  # 排序\n  "),
    actionButton("check", "检查代码"),
    verbatimTextOutput("result")
  )
}

# 完整的dplyr练习
dplyr_exercises <- function() {
  tabsetPanel(
    tabPanel("练习1: mutate函数", dplyr_exercise1()),
    tabPanel("练习2: arrange函数", dplyr_exercise2()),
    tabPanel("练习3: group_by和summarise函数", dplyr_exercise3()),
    tabPanel("练习4: 编程练习", dplyr_exercise4())
  )
} 