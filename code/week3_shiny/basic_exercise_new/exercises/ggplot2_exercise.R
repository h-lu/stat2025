# ggplot2基础练习

library(learnr)
library(tidyverse)
library(palmerpenguins)

# 练习1: 基本图表类型
ggplot2_exercise1 <- function() {
  quiz(
    question("以下哪个代码可以绘制企鹅体重的直方图？",
      answer("ggplot(penguins, aes(x = body_mass_g)) + geom_histogram()", correct = TRUE),
      answer("ggplot(penguins, aes(y = body_mass_g)) + geom_histogram()"),
      answer("ggplot(penguins, aes(x = body_mass_g)) + geom_bar()"),
      answer("ggplot(penguins, aes(x = body_mass_g)) + geom_boxplot()"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以绘制企鹅嘴峰长度与体重的散点图？",
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point()", correct = TRUE),
      answer("ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm)) + geom_line()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_bar()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_histogram()"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以绘制不同种类企鹅体重的箱线图？",
      answer("ggplot(penguins, aes(x = species, y = body_mass_g)) + geom_boxplot()", correct = TRUE),
      answer("ggplot(penguins, aes(x = body_mass_g, y = species)) + geom_boxplot()"),
      answer("ggplot(penguins, aes(x = species, y = body_mass_g)) + geom_point()"),
      answer("ggplot(penguins, aes(x = species, y = body_mass_g)) + geom_histogram()"),
      allow_retry = TRUE
    )
  )
}

# 练习2: 美化图形
ggplot2_exercise2 <- function() {
  quiz(
    question("以下哪个代码可以为图形添加标题和坐标轴标签？",
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + labs(title = \"企鹅嘴峰长度与体重的关系\", x = \"嘴峰长度 (mm)\", y = \"体重 (g)\")", correct = TRUE),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + title(\"企鹅嘴峰长度与体重的关系\") + xlab(\"嘴峰长度 (mm)\") + ylab(\"体重 (g)\")"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, title = \"企鹅嘴峰长度与体重的关系\", x = \"嘴峰长度 (mm)\", y = \"体重 (g)\")) + geom_point()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + ggtitle(\"企鹅嘴峰长度与体重的关系\") + xlab(\"嘴峰长度 (mm)\") + ylab(\"体重 (g)\")", correct = TRUE),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以更改图形的主题？",
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + theme_minimal()", correct = TRUE),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + theme = \"minimal\""),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, theme = \"minimal\")) + geom_point()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + set_theme(\"minimal\")"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以按企鹅种类为散点图着色？",
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) + geom_point()", correct = TRUE),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point(color = species)"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + color(species)"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + scale_color_discrete(species)"),
      allow_retry = TRUE
    )
  )
}

# 练习3: 分面和图层
ggplot2_exercise3 <- function() {
  quiz(
    question("以下哪个代码可以按企鹅性别创建分面图？",
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + facet_wrap(~ sex)", correct = TRUE),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + facet(sex)"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, facet = sex)) + geom_point()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + split(sex)"),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以在散点图上添加回归线？",
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + geom_smooth(method = \"lm\")", correct = TRUE),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + geom_regression()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + add_regression_line()"),
      answer("ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + geom_point() + stat_smooth(method = \"lm\")", correct = TRUE),
      allow_retry = TRUE
    ),
    question("以下哪个代码可以创建堆叠条形图？",
      answer("ggplot(penguins, aes(x = island, fill = species)) + geom_bar()", correct = TRUE),
      answer("ggplot(penguins, aes(x = island, y = species)) + geom_bar()"),
      answer("ggplot(penguins, aes(x = island, color = species)) + geom_bar()"),
      answer("ggplot(penguins, aes(x = island, fill = species)) + geom_col()"),
      allow_retry = TRUE
    )
  )
}

# 练习4: 编程练习
ggplot2_exercise4 <- function() {
  exercise <- exercise_server("ggplot2_code", 
    function(input, output, session) {
      # 检查代码是否正确
      check_code <- function(user_code) {
        # 尝试执行用户代码
        tryCatch({
          # 创建一个新的环境来执行代码
          env <- new.env()
          
          # 在环境中加载数据和包
          env$penguins <- penguins
          env$ggplot <- ggplot
          env$aes <- aes
          env$geom_point <- geom_point
          env$geom_smooth <- geom_smooth
          env$labs <- labs
          env$scale_color_brewer <- scale_color_brewer
          env$theme_minimal <- theme_minimal
          
          # 执行用户代码
          result <- eval(parse(text = user_code), env)
          
          # 检查结果是否是ggplot对象
          if (!inherits(result, "gg")) {
            return(list(success = FALSE, message = "结果应该是一个ggplot对象。"))
          }
          
          # 检查是否包含必要的图层
          layers <- result$layers
          has_point_layer <- any(sapply(layers, function(l) inherits(l$geom, "GeomPoint")))
          has_smooth_layer <- any(sapply(layers, function(l) inherits(l$geom, "GeomSmooth")))
          
          if (!has_point_layer) {
            return(list(success = FALSE, message = "图表应该包含散点图层（geom_point）。"))
          }
          
          if (!has_smooth_layer) {
            return(list(success = FALSE, message = "图表应该包含平滑线图层（geom_smooth）。"))
          }
          
          # 检查是否按species着色
          mapping <- result$mapping
          if (is.null(mapping$colour) || mapping$colour != "species") {
            return(list(success = FALSE, message = "图表应该按企鹅种类（species）着色。"))
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
    p("使用ggplot2函数编写代码，创建一个展示企鹅嘴峰长度与体重关系的散点图，并添加回归线。"),
    p("要求："),
    tags$ul(
      tags$li("使用geom_point()绘制散点图"),
      tags$li("使用geom_smooth(method = \"lm\")添加回归线"),
      tags$li("按企鹅种类（species）着色"),
      tags$li("添加适当的标题和坐标轴标签"),
      tags$li("使用theme_minimal()主题")
    ),
    textAreaInput("ggplot2_code", "代码", rows = 10, 
      value = "# 在这里编写代码\nggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) +\n  # 添加散点图层\n  \n  # 添加回归线\n  \n  # 添加标题和标签\n  \n  # 设置主题\n  "),
    actionButton("check", "检查代码"),
    verbatimTextOutput("result")
  )
}

# 完整的ggplot2练习
ggplot2_exercises <- function() {
  tabsetPanel(
    tabPanel("练习1: 基本图表类型", ggplot2_exercise1()),
    tabPanel("练习2: 美化图形", ggplot2_exercise2()),
    tabPanel("练习3: 分面和图层", ggplot2_exercise3()),
    tabPanel("练习4: 编程练习", ggplot2_exercise4())
  )
} 