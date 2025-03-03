# ggplot2数据可视化演示模块
# 包含基本图表绘制和美化的交互式演示

library(shiny)
library(ggplot2)
library(plotly)

# ggplot2演示UI组件
ggplot2DemoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("ggplot2数据可视化演示"),
        tabsetPanel(
          # 基本图表类型演示面板
          tabPanel("基本图表类型", 
            fluidRow(
              column(4,
                h4("图表类型"),
                selectInput(ns("plot_type"), "选择图表类型：", 
                  choices = c(
                    "直方图 (histogram)" = "histogram",
                    "散点图 (scatter)" = "scatter",
                    "箱线图 (boxplot)" = "boxplot",
                    "条形图 (bar)" = "bar",
                    "折线图 (line)" = "line",
                    "密度图 (density)" = "density"
                  )
                ),
                hr(),
                h4("数据选择"),
                conditionalPanel(
                  condition = "input.plot_type == 'histogram' || input.plot_type == 'density'", 
                  ns = ns,
                  selectInput(ns("hist_var"), "选择变量：", 
                    choices = c(
                      "体重 (body_mass_g)" = "body_mass_g",
                      "嘴峰长度 (bill_length_mm)" = "bill_length_mm",
                      "嘴峰深度 (bill_depth_mm)" = "bill_depth_mm",
                      "鳍长度 (flipper_length_mm)" = "flipper_length_mm"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'scatter'", 
                  ns = ns,
                  selectInput(ns("scatter_x"), "X轴变量：", 
                    choices = c(
                      "嘴峰长度 (bill_length_mm)" = "bill_length_mm",
                      "嘴峰深度 (bill_depth_mm)" = "bill_depth_mm",
                      "鳍长度 (flipper_length_mm)" = "flipper_length_mm",
                      "体重 (body_mass_g)" = "body_mass_g"
                    )
                  ),
                  selectInput(ns("scatter_y"), "Y轴变量：", 
                    choices = c(
                      "体重 (body_mass_g)" = "body_mass_g",
                      "嘴峰长度 (bill_length_mm)" = "bill_length_mm",
                      "嘴峰深度 (bill_depth_mm)" = "bill_depth_mm",
                      "鳍长度 (flipper_length_mm)" = "flipper_length_mm"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'boxplot' || input.plot_type == 'bar'", 
                  ns = ns,
                  selectInput(ns("box_y"), "Y轴变量：", 
                    choices = c(
                      "体重 (body_mass_g)" = "body_mass_g",
                      "嘴峰长度 (bill_length_mm)" = "bill_length_mm",
                      "嘴峰深度 (bill_depth_mm)" = "bill_depth_mm",
                      "鳍长度 (flipper_length_mm)" = "flipper_length_mm"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'line'", 
                  ns = ns,
                  p("折线图将按企鹅种类展示平均体重")
                ),
                hr(),
                h4("分组与美化"),
                checkboxInput(ns("use_color"), "按企鹅种类着色", value = TRUE),
                checkboxInput(ns("use_facet"), "按企鹅性别分面", value = FALSE),
                actionButton(ns("update_plot"), "更新图表", class = "btn-primary")
              ),
              column(8,
                h4("可视化结果"),
                plotlyOutput(ns("plot_result"), height = "500px"),
                hr(),
                h4("代码"),
                verbatimTextOutput(ns("plot_code"))
              )
            )
          ),
          
          # 图形美化演示面板
          tabPanel("图形美化",
            fluidRow(
              column(4,
                h4("基础设置"),
                selectInput(ns("base_plot"), "选择基础图表：", 
                  choices = c(
                    "散点图 - 嘴峰长度与体重" = "scatter",
                    "箱线图 - 不同种类体重分布" = "boxplot",
                    "直方图 - 体重分布" = "histogram"
                  )
                ),
                hr(),
                h4("标题与标签"),
                textInput(ns("plot_title"), "图表标题：", "企鹅数据可视化"),
                textInput(ns("plot_subtitle"), "副标题：", "基于Palmer Penguins数据集"),
                textInput(ns("x_label"), "X轴标签：", ""),
                textInput(ns("y_label"), "Y轴标签：", ""),
                hr(),
                h4("主题设置"),
                selectInput(ns("plot_theme"), "选择主题：", 
                  choices = c(
                    "默认主题" = "theme_gray",
                    "最小主题" = "theme_minimal",
                    "经典主题" = "theme_classic",
                    "黑白主题" = "theme_bw",
                    "暗色主题" = "theme_dark"
                  )
                ),
                hr(),
                h4("颜色设置"),
                selectInput(ns("color_palette"), "颜色方案：", 
                  choices = c(
                    "默认" = "default",
                    "Set1" = "Set1",
                    "Set2" = "Set2",
                    "Set3" = "Set3",
                    "Pastel1" = "Pastel1",
                    "Dark2" = "Dark2"
                  )
                ),
                actionButton(ns("update_style"), "应用样式", class = "btn-primary")
              ),
              column(8,
                h4("美化结果"),
                plotlyOutput(ns("styled_plot"), height = "500px"),
                hr(),
                h4("代码"),
                verbatimTextOutput(ns("style_code"))
              )
            )
          ),
          
          # 交互式图表演示面板
          tabPanel("交互式图表",
            fluidRow(
              column(12,
                h4("使用plotly创建交互式图表"),
                p("plotly包可以将ggplot2图表转换为交互式图表，支持缩放、悬停查看数据点信息等功能。"),
                hr(),
                selectInput(ns("interactive_plot"), "选择图表类型：", 
                  choices = c(
                    "散点图 - 嘴峰长度与体重" = "scatter",
                    "箱线图 - 不同种类体重分布" = "boxplot",
                    "条形图 - 各岛屿企鹅数量" = "bar"
                  )
                ),
                actionButton(ns("show_interactive"), "显示交互式图表", class = "btn-primary"),
                hr(),
                plotlyOutput(ns("plotly_result"), height = "500px"),
                hr(),
                h4("代码"),
                verbatimTextOutput(ns("plotly_code"))
              )
            )
          )
        )
      )
    )
  )
}

# ggplot2演示服务器组件
ggplot2DemoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 加载数据
    penguins_data <- reactive({
      source("modules/data_processing.R")
      load_penguins_data()
    })
    
    # 基本图表类型
    observeEvent(input$update_plot, {
      # 获取数据
      data <- penguins_data()
      
      # 根据选择的图表类型生成代码和图表
      if (input$plot_type == "histogram") {
        var_name <- input$hist_var
        var_label <- names(which(c(
          "body_mass_g" = "体重 (g)",
          "bill_length_mm" = "嘴峰长度 (mm)",
          "bill_depth_mm" = "嘴峰深度 (mm)",
          "flipper_length_mm" = "鳍长度 (mm)"
        ) == var_name))
        
        plot_code <- paste0(
          'ggplot(penguins, aes(x = ', var_name, 
          if (input$use_color) ', fill = species', 
          ')) +\n  geom_histogram(bins = 30, alpha = 0.7',
          if (!input$use_color) ', fill = "#69b3a2"',
          ') +\n  labs(title = "企鹅', var_label, '分布", x = "', var_label, '", y = "频数")',
          if (input$use_facet) ' +\n  facet_wrap(~ sex)',
          if (input$use_color) ' +\n  scale_fill_brewer(palette = "Set2")'
        )
        
        p <- eval(parse(text = plot_code))
        
      } else if (input$plot_type == "scatter") {
        x_var <- input$scatter_x
        y_var <- input$scatter_y
        
        x_label <- names(which(c(
          "body_mass_g" = "体重 (g)",
          "bill_length_mm" = "嘴峰长度 (mm)",
          "bill_depth_mm" = "嘴峰深度 (mm)",
          "flipper_length_mm" = "鳍长度 (mm)"
        ) == x_var))
        
        y_label <- names(which(c(
          "body_mass_g" = "体重 (g)",
          "bill_length_mm" = "嘴峰长度 (mm)",
          "bill_depth_mm" = "嘴峰深度 (mm)",
          "flipper_length_mm" = "鳍长度 (mm)"
        ) == y_var))
        
        plot_code <- paste0(
          'ggplot(penguins, aes(x = ', x_var, ', y = ', y_var,
          if (input$use_color) ', color = species',
          ')) +\n  geom_point(alpha = 0.7, size = 3) +\n  labs(title = "企鹅', x_label, '与', y_label, '的关系", x = "', x_label, '", y = "', y_label, '")',
          if (input$use_facet) ' +\n  facet_wrap(~ sex)',
          if (input$use_color) ' +\n  scale_color_brewer(palette = "Set2")'
        )
        
        p <- eval(parse(text = plot_code))
        
      } else if (input$plot_type == "boxplot") {
        y_var <- input$box_y
        
        y_label <- names(which(c(
          "body_mass_g" = "体重 (g)",
          "bill_length_mm" = "嘴峰长度 (mm)",
          "bill_depth_mm" = "嘴峰深度 (mm)",
          "flipper_length_mm" = "鳍长度 (mm)"
        ) == y_var))
        
        plot_code <- paste0(
          'ggplot(penguins, aes(x = species, y = ', y_var,
          if (input$use_color) ', fill = species',
          ')) +\n  geom_boxplot(alpha = 0.7) +\n  labs(title = "不同种类企鹅的', y_label, '分布", x = "企鹅种类", y = "', y_label, '")',
          if (input$use_facet) ' +\n  facet_wrap(~ sex)',
          if (input$use_color) ' +\n  scale_fill_brewer(palette = "Set2")'
        )
        
        p <- eval(parse(text = plot_code))
        
      } else if (input$plot_type == "bar") {
        y_var <- input$box_y
        
        y_label <- names(which(c(
          "body_mass_g" = "体重 (g)",
          "bill_length_mm" = "嘴峰长度 (mm)",
          "bill_depth_mm" = "嘴峰深度 (mm)",
          "flipper_length_mm" = "鳍长度 (mm)"
        ) == y_var))
        
        plot_code <- paste0(
          'penguins %>%\n  group_by(species) %>%\n  summarise(mean_value = mean(', y_var, ', na.rm = TRUE)) %>%\n  ggplot(aes(x = species, y = mean_value',
          if (input$use_color) ', fill = species',
          ')) +\n  geom_col(alpha = 0.7) +\n  labs(title = "不同种类企鹅的平均', y_label, '", x = "企鹅种类", y = "平均', y_label, '")',
          if (input$use_color) ' +\n  scale_fill_brewer(palette = "Set2")'
        )
        
        p <- eval(parse(text = plot_code))
        
      } else if (input$plot_type == "line") {
        plot_code <- paste0(
          'penguins %>%\n  group_by(species, year) %>%\n  summarise(mean_mass = mean(body_mass_g, na.rm = TRUE), .groups = "drop") %>%\n  ggplot(aes(x = year, y = mean_mass, group = species',
          if (input$use_color) ', color = species',
          ')) +\n  geom_line(size = 1.2) +\n  geom_point(size = 3) +\n  labs(title = "不同年份各种类企鹅的平均体重变化", x = "年份", y = "平均体重 (g)")',
          if (input$use_facet) ' +\n  facet_wrap(~ species, scales = "free_y")',
          if (input$use_color) ' +\n  scale_color_brewer(palette = "Set2")'
        )
        
        p <- eval(parse(text = plot_code))
        
      } else if (input$plot_type == "density") {
        var_name <- input$hist_var
        var_label <- names(which(c(
          "body_mass_g" = "体重 (g)",
          "bill_length_mm" = "嘴峰长度 (mm)",
          "bill_depth_mm" = "嘴峰深度 (mm)",
          "flipper_length_mm" = "鳍长度 (mm)"
        ) == var_name))
        
        plot_code <- paste0(
          'ggplot(penguins, aes(x = ', var_name, 
          if (input$use_color) ', fill = species', 
          ')) +\n  geom_density(alpha = 0.5) +\n  labs(title = "企鹅', var_label, '密度分布", x = "', var_label, '", y = "密度")',
          if (input$use_facet) ' +\n  facet_wrap(~ sex)',
          if (input$use_color) ' +\n  scale_fill_brewer(palette = "Set2")'
        )
        
        p <- eval(parse(text = plot_code))
      }
      
      # 显示代码和图表
      output$plot_code <- renderText({ plot_code })
      output$plot_result <- renderPlotly({ ggplotly(p) })
    })
    
    # 图形美化
    observeEvent(input$update_style, {
      # 获取数据
      data <- penguins_data()
      
      # 根据选择的基础图表生成代码和图表
      if (input$base_plot == "scatter") {
        base_code <- 'ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) +\n  geom_point(alpha = 0.7, size = 3)'
      } else if (input$base_plot == "boxplot") {
        base_code <- 'ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +\n  geom_boxplot(alpha = 0.7)'
      } else if (input$base_plot == "histogram") {
        base_code <- 'ggplot(penguins, aes(x = body_mass_g, fill = species)) +\n  geom_histogram(bins = 30, alpha = 0.7)'
      }
      
      # 添加标题和标签
      labs_code <- paste0(
        ' +\n  labs(\n    title = "', input$plot_title, 
        '",\n    subtitle = "', input$plot_subtitle, '"'
      )
      
      if (input$x_label != "") {
        labs_code <- paste0(labs_code, ',\n    x = "', input$x_label, '"')
      }
      
      if (input$y_label != "") {
        labs_code <- paste0(labs_code, ',\n    y = "', input$y_label, '"')
      }
      
      labs_code <- paste0(labs_code, '\n  )')
      
      # 添加主题
      theme_code <- paste0(' +\n  ', input$plot_theme, '()')
      
      # 添加颜色方案
      if (input$color_palette != "default") {
        if (input$base_plot == "scatter") {
          color_code <- paste0(' +\n  scale_color_brewer(palette = "', input$color_palette, '")')
        } else {
          color_code <- paste0(' +\n  scale_fill_brewer(palette = "', input$color_palette, '")')
        }
      } else {
        color_code <- ""
      }
      
      # 组合完整代码
      full_code <- paste0(base_code, labs_code, theme_code, color_code)
      
      # 执行代码生成图表
      p <- eval(parse(text = full_code))
      
      # 显示代码和图表
      output$style_code <- renderText({ full_code })
      output$styled_plot <- renderPlotly({ ggplotly(p) })
    })
    
    # 交互式图表
    observeEvent(input$show_interactive, {
      # 获取数据
      data <- penguins_data()
      
      # 根据选择的图表类型生成代码和图表
      if (input$interactive_plot == "scatter") {
        plot_code <- 'p <- ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species, text = paste("种类:", species, "<br>嘴峰长度:", bill_length_mm, "mm<br>体重:", body_mass_g, "g"))) +\n  geom_point(alpha = 0.7, size = 3) +\n  labs(title = "企鹅嘴峰长度与体重的关系", x = "嘴峰长度 (mm)", y = "体重 (g)") +\n  scale_color_brewer(palette = "Set2") +\n  theme_minimal()\n\nggplotly(p, tooltip = "text")'
      } else if (input$interactive_plot == "boxplot") {
        plot_code <- 'p <- ggplot(penguins, aes(x = species, y = body_mass_g, fill = species, text = paste("种类:", species, "<br>体重:", body_mass_g, "g"))) +\n  geom_boxplot(alpha = 0.7) +\n  labs(title = "不同种类企鹅的体重分布", x = "企鹅种类", y = "体重 (g)") +\n  scale_fill_brewer(palette = "Set2") +\n  theme_minimal()\n\nggplotly(p, tooltip = "text")'
      } else if (input$interactive_plot == "bar") {
        plot_code <- 'p <- penguins %>%\n  count(island, species) %>%\n  ggplot(aes(x = island, y = n, fill = species, text = paste("岛屿:", island, "<br>种类:", species, "<br>数量:", n))) +\n  geom_col(position = "dodge", alpha = 0.7) +\n  labs(title = "各岛屿不同种类企鹅的数量", x = "岛屿", y = "数量") +\n  scale_fill_brewer(palette = "Set2") +\n  theme_minimal()\n\nggplotly(p, tooltip = "text")'
      }
      
      # 执行代码生成图表
      p <- eval(parse(text = plot_code))
      
      # 显示代码和图表
      output$plotly_code <- renderText({ plot_code })
      output$plotly_result <- renderPlotly({ p })
    })
  })
} 