# dplyr高级函数演示模块
# 包含mutate, arrange, group_by, summarise等函数的交互式演示

library(shiny)
library(DT)

# dplyr演示UI组件
dplyrDemoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("dplyr高级函数演示"),
        tabsetPanel(
          # mutate演示面板
          tabPanel("mutate函数", 
            fluidRow(
              column(6,
                h4("函数说明"),
                p("mutate()函数用于创建新列或修改现有列，可以基于现有列进行计算。"),
                code("mutate(new_column = expression)"),
                hr(),
                selectInput(ns("mutate_example"), "选择示例：", 
                  choices = c(
                    "创建新列 - 计算体重与嘴峰长度比值" = "ratio",
                    "单位转换 - 将体重从克转换为千克" = "unit",
                    "条件创建 - 根据体重分类" = "condition"
                  )
                ),
                actionButton(ns("run_mutate"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("mutate_code")),
                h4("结果"),
                DTOutput(ns("mutate_result"))
              )
            )
          ),
          
          # arrange演示面板
          tabPanel("arrange函数",
            fluidRow(
              column(6,
                h4("函数说明"),
                p("arrange()函数用于对数据进行排序，可以按照一个或多个列进行升序或降序排序。"),
                code("arrange(column1, desc(column2))"),
                hr(),
                selectInput(ns("arrange_example"), "选择示例：", 
                  choices = c(
                    "单列升序排序 - 按体重升序" = "asc",
                    "单列降序排序 - 按体重降序" = "desc",
                    "多列排序 - 按种类和体重排序" = "multi"
                  )
                ),
                actionButton(ns("run_arrange"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("arrange_code")),
                h4("结果"),
                DTOutput(ns("arrange_result"))
              )
            )
          ),
          
          # group_by和summarise演示面板
          tabPanel("group_by & summarise",
            fluidRow(
              column(6,
                h4("函数说明"),
                p("group_by()函数用于按照一个或多个变量对数据进行分组，通常与summarise()函数一起使用，对分组数据进行汇总统计。"),
                code("group_by(column1, column2) %>% summarise(mean = mean(column3))"),
                hr(),
                selectInput(ns("group_example"), "选择示例：", 
                  choices = c(
                    "按种类分组 - 计算平均体重" = "species",
                    "按种类和性别分组 - 计算多个统计量" = "species_sex",
                    "按岛屿分组 - 计算企鹅数量" = "island"
                  )
                ),
                actionButton(ns("run_group"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("group_code")),
                h4("结果"),
                DTOutput(ns("group_result"))
              )
            )
          ),
          
          # 其他dplyr函数演示面板
          tabPanel("其他函数",
            fluidRow(
              column(6,
                h4("函数说明"),
                p("dplyr包还提供了许多其他有用的函数，如distinct()、rename()、slice()等。"),
                hr(),
                selectInput(ns("other_example"), "选择函数：", 
                  choices = c(
                    "distinct() - 去除重复行" = "distinct",
                    "rename() - 重命名列" = "rename",
                    "slice() - 选择特定行" = "slice",
                    "count() - 计数统计" = "count"
                  )
                ),
                actionButton(ns("run_other"), "运行示例", class = "btn-primary")
              ),
              column(6,
                h4("代码"),
                verbatimTextOutput(ns("other_code")),
                h4("结果"),
                DTOutput(ns("other_result"))
              )
            )
          )
        )
      )
    )
  )
}

# dplyr演示服务器组件
dplyrDemoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 加载数据
    penguins_data <- reactive({
      source("modules/data_processing.R")
      load_penguins_data()
    })
    
    # mutate函数示例
    mutate_examples <- list(
      ratio = list(
        code = 'penguins %>%\n  mutate(weight_length_ratio = body_mass_g / bill_length_mm) %>%\n  select(species, bill_length_mm, body_mass_g, weight_length_ratio)',
        func = function(data) {
          data %>%
            mutate(weight_length_ratio = body_mass_g / bill_length_mm) %>%
            select(species, bill_length_mm, body_mass_g, weight_length_ratio)
        }
      ),
      unit = list(
        code = 'penguins %>%\n  mutate(body_mass_kg = body_mass_g / 1000) %>%\n  select(species, body_mass_g, body_mass_kg)',
        func = function(data) {
          data %>%
            mutate(body_mass_kg = body_mass_g / 1000) %>%
            select(species, body_mass_g, body_mass_kg)
        }
      ),
      condition = list(
        code = 'penguins %>%\n  mutate(weight_category = case_when(\n    body_mass_g < 3500 ~ "轻",\n    body_mass_g < 4500 ~ "中",\n    TRUE ~ "重"\n  )) %>%\n  select(species, body_mass_g, weight_category)',
        func = function(data) {
          data %>%
            mutate(weight_category = case_when(
              body_mass_g < 3500 ~ "轻",
              body_mass_g < 4500 ~ "中",
              TRUE ~ "重"
            )) %>%
            select(species, body_mass_g, weight_category)
        }
      )
    )
    
    # arrange函数示例
    arrange_examples <- list(
      asc = list(
        code = 'penguins %>%\n  arrange(body_mass_g) %>%\n  select(species, body_mass_g)',
        func = function(data) {
          data %>%
            arrange(body_mass_g) %>%
            select(species, body_mass_g)
        }
      ),
      desc = list(
        code = 'penguins %>%\n  arrange(desc(body_mass_g)) %>%\n  select(species, body_mass_g)',
        func = function(data) {
          data %>%
            arrange(desc(body_mass_g)) %>%
            select(species, body_mass_g)
        }
      ),
      multi = list(
        code = 'penguins %>%\n  arrange(species, desc(body_mass_g)) %>%\n  select(species, body_mass_g)',
        func = function(data) {
          data %>%
            arrange(species, desc(body_mass_g)) %>%
            select(species, body_mass_g)
        }
      )
    )
    
    # group_by和summarise函数示例
    group_examples <- list(
      species = list(
        code = 'penguins %>%\n  group_by(species) %>%\n  summarise(\n    平均体重 = mean(body_mass_g, na.rm = TRUE),\n    样本数 = n()\n  )',
        func = function(data) {
          data %>%
            group_by(species) %>%
            summarise(
              平均体重 = mean(body_mass_g, na.rm = TRUE),
              样本数 = n()
            )
        }
      ),
      species_sex = list(
        code = 'penguins %>%\n  group_by(species, sex) %>%\n  summarise(\n    平均体重 = mean(body_mass_g, na.rm = TRUE),\n    平均嘴峰长度 = mean(bill_length_mm, na.rm = TRUE),\n    样本数 = n(),\n    .groups = "drop"\n  )',
        func = function(data) {
          data %>%
            group_by(species, sex) %>%
            summarise(
              平均体重 = mean(body_mass_g, na.rm = TRUE),
              平均嘴峰长度 = mean(bill_length_mm, na.rm = TRUE),
              样本数 = n(),
              .groups = "drop"
            )
        }
      ),
      island = list(
        code = 'penguins %>%\n  group_by(island) %>%\n  summarise(\n    企鹅数量 = n(),\n    种类数 = n_distinct(species)\n  )',
        func = function(data) {
          data %>%
            group_by(island) %>%
            summarise(
              企鹅数量 = n(),
              种类数 = n_distinct(species)
            )
        }
      )
    )
    
    # 其他dplyr函数示例
    other_examples <- list(
      distinct = list(
        code = 'penguins %>%\n  distinct(species, island) %>%\n  arrange(species)',
        func = function(data) {
          data %>%
            distinct(species, island) %>%
            arrange(species)
        }
      ),
      rename = list(
        code = 'penguins %>%\n  rename(\n    嘴峰长度 = bill_length_mm,\n    嘴峰深度 = bill_depth_mm\n  ) %>%\n  select(species, 嘴峰长度, 嘴峰深度)',
        func = function(data) {
          data %>%
            rename(
              嘴峰长度 = bill_length_mm,
              嘴峰深度 = bill_depth_mm
            ) %>%
            select(species, 嘴峰长度, 嘴峰深度)
        }
      ),
      slice = list(
        code = 'penguins %>%\n  slice(1:5)',
        func = function(data) {
          data %>%
            slice(1:5)
        }
      ),
      count = list(
        code = 'penguins %>%\n  count(species, sex, sort = TRUE)',
        func = function(data) {
          data %>%
            count(species, sex, sort = TRUE)
        }
      )
    )
    
    # 显示mutate代码和结果
    observeEvent(input$run_mutate, {
      example <- mutate_examples[[input$mutate_example]]
      output$mutate_code <- renderText({ example$code })
      output$mutate_result <- renderDT({
        example$func(penguins_data())
      }, options = list(pageLength = 5))
    })
    
    # 显示arrange代码和结果
    observeEvent(input$run_arrange, {
      example <- arrange_examples[[input$arrange_example]]
      output$arrange_code <- renderText({ example$code })
      output$arrange_result <- renderDT({
        example$func(penguins_data())
      }, options = list(pageLength = 5))
    })
    
    # 显示group_by和summarise代码和结果
    observeEvent(input$run_group, {
      example <- group_examples[[input$group_example]]
      output$group_code <- renderText({ example$code })
      output$group_result <- renderDT({
        example$func(penguins_data())
      }, options = list(pageLength = 5))
    })
    
    # 显示其他函数代码和结果
    observeEvent(input$run_other, {
      example <- other_examples[[input$other_example]]
      output$other_code <- renderText({ example$code })
      output$other_result <- renderDT({
        example$func(penguins_data())
      }, options = list(pageLength = 5))
    })
  })
} 