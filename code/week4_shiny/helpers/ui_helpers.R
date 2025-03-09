# UI辅助函数
# 包含用于创建常用UI元素的函数

# 创建标题卡片
create_title_card <- function(title, subtitle = NULL, icon = NULL, color = app_colors$chart_primary) {
  box(
    width = 12,
    title = NULL,
    status = "primary",
    solidHeader = FALSE,
    
    div(
      class = "text-center",
      style = "padding: 20px 10px; margin-bottom: 20px; background: white; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.05);",
      
      if (!is.null(icon)) {
        div(icon(icon, style = paste0("color: ", color, "; font-size: 48px; margin-bottom: 15px;")))
      },
      
      h2(style = "font-weight: 700; font-family: 'notosans'; margin-bottom: 10px; color: #333;", title),
      
      if (!is.null(subtitle)) {
        h4(style = "font-weight: 400; font-family: 'notosans'; color: #666;", subtitle)
      }
    )
  )
}

# 创建信息卡片
create_info_card <- function(title, value, icon, color = app_colors$chart_primary) {
  div(
    class = "info-box",
    style = paste0("background: white; border-radius: 8px; margin-bottom: 15px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); overflow: hidden; border-left: 4px solid ", color, ";"),
    
    div(
      class = "info-box-icon",
      style = paste0("background-color: ", color, "; color: white; padding: 15px; display: flex; align-items: center; justify-content: center;"),
      icon(icon, style = "font-size: 24px;")
    ),
    
    div(
      class = "info-box-content",
      style = "padding: 15px;",
      div(class = "info-box-text", style = "font-weight: 500; color: #666; font-size: 14px; font-family: 'roboto';", title),
      div(class = "info-box-number", style = "font-weight: 700; color: #333; font-size: 24px; font-family: 'roboto';", value)
    )
  )
}

# 创建分隔线
create_divider <- function(color = app_colors$bg_light) {
  div(
    style = paste0("height: 1px; background-color: ", color, "; margin: 20px 0;")
  )
}

# 创建统计卡片
create_stat_card <- function(title, value, subtitle = NULL, color = app_colors$chart_primary, width = 4) {
  div(
    class = "col-md-{width}",
    div(
      class = "stat-card",
      style = paste0("background: white; border-radius: 8px; padding: 20px; margin-bottom: 15px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); border-top: 3px solid ", color, ";"),
      
      h3(style = "font-weight: 700; font-family: 'roboto'; margin-bottom: 5px; font-size: 28px; color: #333;", value),
      p(style = "font-weight: 500; font-family: 'roboto'; color: #666; font-size: 16px; margin-bottom: 10px;", title),
      
      if (!is.null(subtitle)) {
        p(style = "font-size: 14px; color: #999; font-family: 'roboto';", subtitle)
      }
    )
  )
}

# 创建结果展示框
create_result_box <- function(title, content, status = "primary", collapsed = FALSE, width = 12) {
  box(
    width = width,
    title = title,
    status = status,
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = collapsed,
    
    div(
      style = "padding: 10px 0;",
      content
    )
  )
}

# 创建警告/提示标签
create_alert <- function(text, type = "info") {
  div(
    class = paste0("alert alert-", type),
    p(text)
  )
}

# 创建带标签的单选按钮组
create_radio_buttons_with_label <- function(inputId, label, choices, selected = NULL) {
  div(
    div(
      style = "margin-bottom: 5px;",
      strong(label)
    ),
    radioButtons(inputId, NULL, choices, selected)
  )
}

# 创建带标签的滑块输入
create_slider_with_label <- function(inputId, label, min, max, value, step = 1) {
  div(
    div(
      style = "margin-bottom: 5px;",
      strong(label)
    ),
    sliderInput(inputId, NULL, min, max, value, step)
  )
}

# 创建带有解释说明的输入控件
create_input_with_tooltip <- function(input_element, tooltip) {
  div(
    style = "position: relative;",
    input_element,
    tags$span(
      style = "position: absolute; right: 10px; top: 5px; color: #3c8dbc; cursor: pointer;",
      icon("question-circle"),
      title = tooltip
    )
  )
}

# 创建练习题卡片
create_exercise_card <- function(id, title, content, hint = NULL, solution = NULL) {
  div(
    class = "exercise-card",
    style = "background: white; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 3px 10px rgba(0,0,0,0.08); overflow: hidden;",
    
    # 标题栏
    div(
      class = "exercise-header",
      style = "background-color: #f6f6f6; padding: 15px 20px; border-bottom: 1px solid #eee;",
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        h3(style = "margin: 0; font-weight: 600; font-family: 'notosans'; color: #333;", 
           paste0("练习 ", id, ": ", title)),
        span(style = "background: #3498db; color: white; padding: 5px 10px; border-radius: 30px; font-size: 12px; font-weight: 600;", "练习")
      )
    ),
    
    # 内容
    div(
      class = "exercise-content",
      style = "padding: 20px;",
      content
    ),
    
    # 提示和解答按钮
    div(
      class = "exercise-actions",
      style = "padding: 10px 20px 20px; display: flex; gap: 10px;",
      
      if (!is.null(hint)) {
        actionButton(
          paste0("hint_", id),
          "显示提示",
          icon = icon("lightbulb"),
          style = "background-color: #f1c40f; color: white; border: none; border-radius: 4px; padding: 8px 15px;"
        )
      },
      
      if (!is.null(solution)) {
        actionButton(
          paste0("solution_", id),
          "显示解答",
          icon = icon("check-circle"),
          style = "background-color: #2ecc71; color: white; border: none; border-radius: 4px; padding: 8px 15px;"
        )
      }
    )
  )
}

# 创建可切换的标签页
create_tabbed_box <- function(id, title, tabs, status = "primary", width = 12) {
  box(
    width = width,
    title = title,
    status = status,
    solidHeader = TRUE,
    
    div(
      style = "background: white; padding-top: 10px;",
      do.call(tabsetPanel, c(id = id, tabs))
    )
  )
}

# 创建数据表格
create_data_table <- function(data, title = NULL, digits = 3) {
  if (!is.null(title)) {
    div(
      h4(title),
      renderDT({
        datatable(
          data,
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 15, 20),
            dom = 'ltip'
          ),
          rownames = TRUE
        ) %>%
        formatRound(columns = sapply(data, is.numeric), digits = digits)
      })
    )
  } else {
    renderDT({
      datatable(
        data,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          dom = 'ltip'
        ),
        rownames = TRUE
      ) %>%
      formatRound(columns = sapply(data, is.numeric), digits = digits)
    })
  }
}

# 创建图表框
create_plot_box <- function(title, plot_output_id, height = "400px", width = 12) {
  div(
    class = "col-md-{width}",
    div(
      class = "plot-box",
      style = "background: white; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 3px 10px rgba(0,0,0,0.08); overflow: hidden;",
      
      # 标题栏
      div(
        class = "plot-header",
        style = "padding: 15px 20px; border-bottom: 1px solid #eee;",
        h3(style = "margin: 0; font-weight: 600; font-family: 'notosans'; color: #333;", title)
      ),
      
      # 图表
      div(
        class = "plot-content",
        style = "padding: 20px;",
        plotOutput(plot_output_id, height = height)
      )
    )
  )
}

# 创建警告/提示框
create_alert_box <- function(type, content, icon_name = NULL) {
  # 根据类型设置样式
  styles <- list(
    info = list(bg = "#d1ecf1", border = "#bee5eb", text = "#0c5460", icon = "info-circle"),
    success = list(bg = "#d4edda", border = "#c3e6cb", text = "#155724", icon = "check-circle"),
    warning = list(bg = "#fff3cd", border = "#ffeeba", text = "#856404", icon = "exclamation-triangle"),
    danger = list(bg = "#f8d7da", border = "#f5c6cb", text = "#721c24", icon = "times-circle")
  )
  
  style <- styles[[type]]
  if (is.null(icon_name)) icon_name <- style$icon
  
  div(
    class = paste0("alert alert-", type),
    style = paste0("background-color: ", style$bg, "; border: 1px solid ", style$border, 
                  "; color: ", style$text, "; padding: 15px; border-radius: 6px; margin: 15px 0;"),
    div(
      style = "display: flex; align-items: flex-start;",
      div(
        style = "margin-right: 10px; font-size: 18px;",
        icon(icon_name)
      ),
      div(content)
    )
  )
} 