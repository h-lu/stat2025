# 第三周：描述性统计 - 数据探索与可视化
# 综合练习应用

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(palmerpenguins)

# 加载知识图谱函数
source("knowledge_map.R")

# 定义UI
ui <- dashboardPage(
  # 仪表板标题
  dashboardHeader(title = "第三周：综合练习"),
  
  # 侧边栏
  dashboardSidebar(
    sidebarMenu(
      menuItem("练习说明", tabName = "intro", icon = icon("info-circle")),
      menuItem("知识点复习", tabName = "knowledge", icon = icon("project-diagram")),
      menuItem("案例1：企鹅数据分析", tabName = "penguin_case", icon = icon("tasks")),
      menuItem("案例2：学生成绩分析", tabName = "student_case", icon = icon("graduation-cap")),
      menuItem("提交作业", tabName = "submit", icon = icon("paper-plane"))
    )
  ),
  
  # 主体内容
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .process-step {
          padding: 10px;
          margin-bottom: 10px;
          border-left: 3px solid #3c8dbc;
          background-color: #f8f9fa;
        }
        .step-number {
          display: inline-block;
          width: 30px;
          height: 30px;
          line-height: 30px;
          text-align: center;
          border-radius: 50%;
          background-color: #3c8dbc;
          color: white;
          margin-right: 10px;
        }
        .key-concept {
          padding: 15px;
          margin: 10px 0;
          border-left: 5px solid #00a65a;
          background-color: #f8f9fa;
        }
        .concept-title {
          font-weight: bold;
          font-size: 16px;
          color: #00a65a;
          margin-bottom: 8px;
        }
        .process-flow {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin: 20px 0;
          padding: 20px;
          background-color: #f8f9fa;
          border-radius: 5px;
        }
        .process-step {
          background-color: #3c8dbc;
          color: white;
          padding: 10px 20px;
          border-radius: 5px;
          text-align: center;
          min-width: 100px;
        }
        .process-arrow {
          color: #3c8dbc;
          font-size: 24px;
          font-weight: bold;
        }
        .knowledge-map {
          background-color: #f8f9fa;
          border-radius: 5px;
          padding: 20px;
          margin: 10px 0;
        }
        .map-title {
          font-size: 18px;
          font-weight: bold;
          color: #3c8dbc;
          margin-bottom: 15px;
          text-align: center;
        }
        .map-content {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          gap: 15px;
        }
        .map-item {
          background-color: white;
          padding: 10px;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          text-align: center;
        }
        .rubric-table {
          width: 100%;
          max-width: 600px;
          margin: 20px auto;
          background-color: white;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          overflow: hidden;
        }
        .rubric-header {
          background-color: #3c8dbc;
          color: white;
          padding: 15px;
          text-align: center;
          font-size: 18px;
          font-weight: bold;
        }
        .rubric-row {
          display: flex;
          border-bottom: 1px solid #eee;
        }
        .rubric-row:last-child {
          border-bottom: none;
        }
        .rubric-item {
          flex: 2;
          padding: 15px;
          background-color: #f8f9fa;
        }
        .rubric-score {
          flex: 1;
          padding: 15px;
          text-align: center;
          font-weight: bold;
          color: #3c8dbc;
          background-color: white;
        }
      "))
    ),
    
    tabItems(
      # 知识点复习页面
      tabItem(tabName = "knowledge",
        fluidRow(
          box(
            title = "数据分析核心概念", width = 12, status = "primary",
            p("在开始综合练习之前，请复习以下数据分析的核心概念和工具。这些知识将帮助你完成案例分析。")
          )
        ),
        fluidRow(
          box(
            title = "数据分析流程", width = 12, status = "success",
            p("数据分析通常遵循以下流程："),
            div(class = "process-flow",
              div(class = "process-step", "数据获取"),
              div(class = "process-arrow", "→"),
              div(class = "process-step", "数据清洗"),
              div(class = "process-arrow", "→"),
              div(class = "process-step", "数据转换"),
              div(class = "process-arrow", "→"),
              div(class = "process-step", "数据可视化"),
              div(class = "process-arrow", "→"),
              div(class = "process-step", "结果解释")
            ),
            p("在本综合练习中，你将完整地实践这一流程。")
          )
        ),
        fluidRow(
          tabBox(
            title = "核心软件包知识图谱", width = 12,
            tabPanel("dplyr数据处理", 
              p("dplyr 是 tidyverse 中用于数据操作的主要包，掌握这些函数将使你的数据处理更高效："),
              div(class = "knowledge-map",
                div(class = "map-title", "dplyr核心函数"),
                div(class = "map-content",
                  div(class = "map-item", "数据筛选：filter()"),
                  div(class = "map-item", "数据排序：arrange()"),
                  div(class = "map-item", "变量选择：select()"),
                  div(class = "map-item", "变量重命名：rename()"),
                  div(class = "map-item", "变量创建：mutate()"),
                  div(class = "map-item", "数据汇总：summarise()"),
                  div(class = "map-item", "数据分组：group_by()")
                )
              )
            ),
            tabPanel("tidyr数据整理", 
              p("tidyr 专注于数据形状的转换，帮助你创建适合分析的整洁数据："),
              div(class = "knowledge-map",
                div(class = "map-title", "tidyr核心函数"),
                div(class = "map-content",
                  div(class = "map-item", "宽转长：pivot_longer()"),
                  div(class = "map-item", "长转宽：pivot_wider()"),
                  div(class = "map-item", "列拆分：separate()"),
                  div(class = "map-item", "列合并：unite()"),
                  div(class = "map-item", "缺失值处理：drop_na()"),
                  div(class = "map-item", "填充缺失值：fill()")
                )
              )
            ),
            tabPanel("ggplot2数据可视化", 
              p("ggplot2 的图层语法使你能够创建复杂的数据可视化："),
              div(class = "knowledge-map",
                div(class = "map-title", "ggplot2核心组件"),
                div(class = "map-content",
                  div(class = "map-item", "数据层：ggplot()"),
                  div(class = "map-item", "几何对象：geom_*()"),
                  div(class = "映射关系：aes()"),
                  div(class = "统计变换：stat_*()"),
                  div(class = "坐标系统：coord_*()"),
                  div(class = "分面：facet_*()"),
                  div(class = "主题：theme()")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "核心概念回顾", width = 12, status = "info",
            div(class = "key-concept",
              div(class = "concept-title", "整洁数据 (Tidy Data)"),
              p("整洁数据的特点："),
              tags$ul(
                tags$li("每个变量形成一列"),
                tags$li("每个观测值形成一行"),
                tags$li("每个单元格只包含一个值")
              ),
              p("整洁数据便于使用tidyverse工具进行分析和可视化。案例练习中，你需要将原始数据转换为整洁格式。")
            ),
            div(class = "key-concept",
              div(class = "concept-title", "数据转换 (Data Transformation)"),
              p("常见的数据转换操作："),
              tags$ul(
                tags$li("筛选观察值 (filter)"),
                tags$li("创建新变量 (mutate)"),
                tags$li("分组汇总 (group_by + summarize)"),
                tags$li("排序数据 (arrange)"),
                tags$li("连接数据集 (join)")
              ),
              p("在案例分析中，你将进行多种数据转换，以发现数据中的模式和关系。")
            ),
            div(class = "key-concept",
              div(class = "concept-title", "可视化图层 (Visualization Layers)"),
              p("ggplot2的图层构建方法："),
              tags$ol(
                tags$li("建立图表基础 (ggplot)"),
                tags$li("添加几何对象 (geom_*)"),
                tags$li("设置映射关系 (aes)"),
                tags$li("添加标题和标签 (labs)"),
                tags$li("调整主题 (theme)")
              ),
              p("通过添加不同的图层，你可以创建信息丰富、视觉吸引力强的数据可视化。")
            )
          )
        )
      ),
      
      # 练习说明页面
      tabItem(tabName = "intro",
        fluidRow(
          box(
            title = "综合练习说明", width = 12, status = "primary",
            p("本应用包含了第三周课程内容的综合练习，要求你综合运用所学的数据清洗、整理和可视化技能，完成数据分析案例。"),
            p("综合练习包含两个案例："),
            tags$ol(
              tags$li(strong("企鹅数据分析："), "对Palmer企鹅数据集进行探索性分析，包括数据清洗、转换和可视化。"),
              tags$li(strong("学生成绩分析："), "分析学生成绩数据，进行数据整理和可视化。")
            ),
            p("每个案例都包含多个步骤，你需要按照要求完成相应的R代码。完成所有步骤后，将结果截图与代码一起提交。"),
            p("通过完成这些综合练习，你将能够："),
            tags$ul(
              tags$li("综合运用dplyr、tidyr和ggplot2等包进行数据分析"),
              tags$li("培养解决实际数据分析问题的能力"),
              tags$li("理解数据分析的完整流程，从数据清洗到可视化")
            )
          )
        ),
        fluidRow(
          box(
            title = "数据分析流程", width = 12, status = "info",
            div(class = "text-center",
                tags$svg(
                  xmlns = "http://www.w3.org/2000/svg",
                  viewBox = "0 0 800 200",
                  width = "100%",
                  height = "200",
                  tags$style("
                    .process-box { fill: #3c8dbc; stroke: #2c6da0; stroke-width: 2; }
                    .process-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; }
                    .process-arrow { fill: #2c6da0; }
                  "),
                  # 数据获取框
                  tags$rect(class = "process-box", x = "50", y = "75", width = "120", height = "50", rx = "10"),
                  tags$text(class = "process-text", x = "110", y = "105", "数据获取"),
                  # 箭头1
                  tags$polygon(class = "process-arrow", points = "180,100 200,90 200,110"),
                  tags$line(x1 = "170", y1 = "100", x2 = "200", y2 = "100", stroke = "#2c6da0", "stroke-width" = "2"),
                  
                  # 数据清洗框
                  tags$rect(class = "process-box", x = "210", y = "75", width = "120", height = "50", rx = "10"),
                  tags$text(class = "process-text", x = "270", y = "105", "数据清洗"),
                  # 箭头2
                  tags$polygon(class = "process-arrow", points = "340,100 360,90 360,110"),
                  tags$line(x1 = "330", y1 = "100", x2 = "360", y2 = "100", stroke = "#2c6da0", "stroke-width" = "2"),
                  
                  # 数据转换框
                  tags$rect(class = "process-box", x = "370", y = "75", width = "120", height = "50", rx = "10"),
                  tags$text(class = "process-text", x = "430", y = "105", "数据转换"),
                  # 箭头3
                  tags$polygon(class = "process-arrow", points = "500,100 520,90 520,110"),
                  tags$line(x1 = "490", y1 = "100", x2 = "520", y2 = "100", stroke = "#2c6da0", "stroke-width" = "2"),
                  
                  # 数据可视化框
                  tags$rect(class = "process-box", x = "530", y = "75", width = "120", height = "50", rx = "10"),
                  tags$text(class = "process-text", x = "590", y = "105", "数据可视化"),
                  # 箭头4
                  tags$polygon(class = "process-arrow", points = "660,100 680,90 680,110"),
                  tags$line(x1 = "650", y1 = "100", x2 = "680", y2 = "100", stroke = "#2c6da0", "stroke-width" = "2"),
                  
                  # 结果解释框
                  tags$rect(class = "process-box", x = "690", y = "75", width = "120", height = "50", rx = "10"),
                  tags$text(class = "process-text", x = "750", y = "105", "结果解释")
                )
            ),
            p("在本综合练习中，你将完成上述数据分析流程的各个步骤，培养系统的数据分析思维。")
          )
        )
      ),
      
      # 企鹅数据分析案例
      tabItem(tabName = "penguin_case",
        fluidRow(
          box(
            title = "案例1：企鹅数据分析", width = 12, status = "primary",
            p("本案例要求你对Palmer企鹅数据集进行全面分析，回答一系列关于企鹅生物特征的问题。"),
            p("数据集预览："),
            DTOutput("penguin_preview")
          )
        ),
        fluidRow(
          box(
            title = "步骤1：数据清洗", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "1"),
                "检查企鹅数据集的缺失值，并进行适当的处理。"
            ),
            p("要求："),
            tags$ul(
              tags$li("统计每个变量的缺失值数量"),
              tags$li("去除包含缺失值的行或使用适当的方法填补缺失值"),
              tags$li("检查处理后的数据集结构")
            ),
            verbatimTextOutput("penguin_task1_hint"),
            textAreaInput("penguin_task1_code", "在此编写代码", rows = 6),
            actionButton("penguin_task1_check", "运行代码", class = "btn-primary"),
            verbatimTextOutput("penguin_task1_result")
          )
        ),
        fluidRow(
          box(
            title = "步骤2：数据转换", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "2"),
                "创建新变量并进行数据汇总。"
            ),
            p("要求："),
            tags$ul(
              tags$li("创建一个新变量'body_mass_kg'，将体重从克转换为千克"),
              tags$li("创建一个新变量'bill_ratio'，计算嘴峰长度与深度的比值"),
              tags$li("按种类和性别分组，计算各组的平均体重、平均嘴峰长度、平均嘴峰深度和样本数")
            ),
            verbatimTextOutput("penguin_task2_hint"),
            textAreaInput("penguin_task2_code", "在此编写代码", rows = 6),
            actionButton("penguin_task2_check", "运行代码", class = "btn-primary"),
            verbatimTextOutput("penguin_task2_result")
          )
        ),
        fluidRow(
          box(
            title = "步骤3：数据可视化", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "3"),
                "创建多个可视化图表，探索企鹅特征之间的关系。"
            ),
            p("要求："),
            tags$ul(
              tags$li("创建一个散点图，展示嘴峰长度与体重的关系，按种类着色"),
              tags$li("为每个种类添加回归线"),
              tags$li("创建一个箱线图，比较不同种类企鹅在不同岛屿上的体重分布"),
              tags$li("创建一个直方图，展示嘴峰长度的分布，按种类分面")
            ),
            p("图表要求："),
            tags$ul(
              tags$li("添加合适的标题和坐标轴标签"),
              tags$li("使用适当的主题"),
              tags$li("调整图例位置")
            ),
            verbatimTextOutput("penguin_task3_hint"),
            textAreaInput("penguin_task3_code", "在此编写代码 (图表1)", rows = 6),
            actionButton("penguin_task3_1_check", "绘制图表1", class = "btn-primary"),
            plotOutput("penguin_plot1"),
            br(),
            textAreaInput("penguin_task3_code2", "在此编写代码 (图表2)", rows = 6),
            actionButton("penguin_task3_2_check", "绘制图表2", class = "btn-primary"),
            plotOutput("penguin_plot2"),
            br(),
            textAreaInput("penguin_task3_code3", "在此编写代码 (图表3)", rows = 6),
            actionButton("penguin_task3_3_check", "绘制图表3", class = "btn-primary"),
            plotOutput("penguin_plot3")
          )
        ),
        fluidRow(
          box(
            title = "步骤4：数据探索与问题回答", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "4"),
                "基于前面的分析，回答以下问题。"
            ),
            p("问题："),
            tags$ol(
              tags$li("哪个种类的企鹅体重最重？各种类的体重有显著差异吗？"),
              tags$li("企鹅的嘴峰长度与体重之间是否存在相关性？这种相关性在不同种类间是否一致？"),
              tags$li("不同岛屿上的企鹅特征是否有差异？可能的原因是什么？")
            ),
            p("要求："),
            tags$ul(
              tags$li("使用适当的统计方法回答问题"),
              tags$li("结合可视化结果进行解释"),
              tags$li("提出可能的假设或解释")
            ),
            textAreaInput("penguin_task4_answer", "在此回答问题", rows = 10),
            actionButton("penguin_task4_check", "提交答案", class = "btn-primary")
          )
        )
      ),
      
      # 学生成绩分析案例
      tabItem(tabName = "student_case",
        fluidRow(
          box(
            title = "案例2：学生成绩分析", width = 12, status = "primary",
            p("本案例要求你分析一组学生成绩数据，探索不同课程和学期之间的成绩变化。"),
            p("数据集预览："),
            DTOutput("student_preview")
          )
        ),
        fluidRow(
          box(
            title = "步骤1：数据整理", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "1"),
                "对学生成绩数据进行整理，转换格式以便于分析。"
            ),
            p("要求："),
            tags$ul(
              tags$li("将宽格式数据转换为长格式，便于分析不同课程的成绩"),
              tags$li("创建新的分类变量，将成绩分为'优秀'(≥90)、'良好'(≥80)、'中等'(≥70)、'及格'(≥60)和'不及格'(<60)"),
              tags$li("拆分学期列，提取学年和学期信息")
            ),
            verbatimTextOutput("student_task1_hint"),
            textAreaInput("student_task1_code", "在此编写代码", rows = 6),
            actionButton("student_task1_check", "运行代码", class = "btn-primary"),
            verbatimTextOutput("student_task1_result")
          )
        ),
        fluidRow(
          box(
            title = "步骤2：数据汇总", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "2"),
                "对学生成绩数据进行汇总统计。"
            ),
            p("要求："),
            tags$ul(
              tags$li("计算每个课程的平均成绩、中位数、最高分和最低分"),
              tags$li("计算每个学生在不同学期的平均成绩"),
              tags$li("计算每个成绩等级的学生人数和百分比")
            ),
            verbatimTextOutput("student_task2_hint"),
            textAreaInput("student_task2_code", "在此编写代码", rows = 6),
            actionButton("student_task2_check", "运行代码", class = "btn-primary"),
            verbatimTextOutput("student_task2_result")
          )
        ),
        fluidRow(
          box(
            title = "步骤3：数据可视化", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "3"),
                "创建多个可视化图表，探索学生成绩的分布和趋势。"
            ),
            p("要求："),
            tags$ul(
              tags$li("创建一个条形图，展示不同课程的平均成绩"),
              tags$li("创建一个热图，展示每个学生在不同课程上的成绩"),
              tags$li("创建一个折线图，展示不同学期的平均成绩变化趋势")
            ),
            verbatimTextOutput("student_task3_hint"),
            textAreaInput("student_task3_code", "在此编写代码 (图表1)", rows = 6),
            actionButton("student_task3_1_check", "绘制图表1", class = "btn-primary"),
            plotOutput("student_plot1"),
            br(),
            textAreaInput("student_task3_code2", "在此编写代码 (图表2)", rows = 6),
            actionButton("student_task3_2_check", "绘制图表2", class = "btn-primary"),
            plotOutput("student_plot2"),
            br(),
            textAreaInput("student_task3_code3", "在此编写代码 (图表3)", rows = 6),
            actionButton("student_task3_3_check", "绘制图表3", class = "btn-primary"),
            plotOutput("student_plot3")
          )
        ),
        fluidRow(
          box(
            title = "步骤4：分析与解释", width = 12, status = "info",
            div(class = "process-step",
                span(class = "step-number", "4"),
                "基于前面的分析，回答以下问题。"
            ),
            p("问题："),
            tags$ol(
              tags$li("哪门课程的平均成绩最高？最低？可能的原因是什么？"),
              tags$li("学生的成绩随着学期的推移有什么变化趋势？"),
              tags$li("不同课程的成绩分布是否存在明显差异？这反映了什么问题？")
            ),
            p("要求："),
            tags$ul(
              tags$li("基于数据分析结果回答问题"),
              tags$li("提出可能的解释或建议"),
              tags$li("思考如何利用这些发现改进教学")
            ),
            textAreaInput("student_task4_answer", "在此回答问题", rows = 10),
            actionButton("student_task4_check", "提交答案", class = "btn-primary")
          )
        )
      ),
      
      # 提交作业页面
      tabItem(tabName = "submit",
        fluidRow(
          box(
            title = "作业提交说明", width = 12, status = "primary",
            p("完成上述两个案例后，请将你的分析结果和代码整理为一份报告，并按照以下要求提交："),
            tags$ol(
              tags$li("将所有代码整理为一个R脚本文件"),
              tags$li("将所有图表保存为高质量图片"),
              tags$li("编写一份分析报告，包含问题描述、数据探索、分析结果和结论"),
              tags$li("将上述文件打包，命名为\"学号_姓名_第三周作业.zip\""),
              tags$li("通过课程网站的作业提交系统上传文件")
            )
          )
        ),
        fluidRow(
          box(
            title = "作业评分标准", width = 12, status = "info",
            div(class = "rubric-table",
              div(class = "rubric-header", "作业评分标准"),
              div(class = "rubric-row",
                div(class = "rubric-item", "数据清洗与整理"),
                div(class = "rubric-score", "25%")
              ),
              div(class = "rubric-row",
                div(class = "rubric-item", "数据转换与汇总"),
                div(class = "rubric-score", "25%")
              ),
              div(class = "rubric-row",
                div(class = "rubric-item", "数据可视化"),
                div(class = "rubric-score", "30%")
              ),
              div(class = "rubric-row",
                div(class = "rubric-item", "分析与解释"),
                div(class = "rubric-score", "20%")
              ),
              div(class = "rubric-row",
                div(class = "rubric-item", "报告格式与组织"),
                div(class = "rubric-score", "10%")
              )
            ),
            p("作业截止日期：2024年3月15日 23:59"),
            p("如有任何问题，请在课程论坛上提问或联系助教。")
          )
        )
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  # 加载企鹅数据
  penguins_data <- reactive({
    palmerpenguins::penguins
  })
  
  # 创建学生成绩数据
  student_data <- reactive({
    # 创建一个模拟的学生成绩数据集
    set.seed(123)
    students <- paste0("学生", 1:20)
    
    # 创建宽格式数据
    data.frame(
      学生 = students,
      学号 = paste0("2023", str_pad(1:20, 4, pad = "0")),
      `2023秋_数学` = round(rnorm(20, mean = 75, sd = 10)),
      `2023秋_语文` = round(rnorm(20, mean = 80, sd = 8)),
      `2023秋_英语` = round(rnorm(20, mean = 78, sd = 12)),
      `2024春_数学` = round(rnorm(20, mean = 78, sd = 10)),
      `2024春_语文` = round(rnorm(20, mean = 82, sd = 8)),
      `2024春_英语` = round(rnorm(20, mean = 80, sd = 12))
    ) %>%
      # 确保成绩在合理范围内
      mutate(across(contains("_"), ~pmin(pmax(.x, 40), 100)))
  })
  
  # 数据预览
  output$penguin_preview <- renderDT({
    datatable(head(penguins_data(), 10), options = list(scrollX = TRUE))
  })
  
  output$student_preview <- renderDT({
    datatable(head(student_data(), 10), options = list(scrollX = TRUE))
  })
  
  # 知识图谱渲染
  output$data_analysis_flow <- renderUI({
    HTML(data_analysis_flow())
  })
  
  output$data_analysis_flow_svg <- renderUI({
    HTML('
      <div class="text-center">
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 200" width="100%" height="200">
          <style>
            .process-box { fill: #3c8dbc; stroke: #2c6da0; stroke-width: 2; }
            .process-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; }
            .process-arrow { fill: #2c6da0; }
          </style>
          <!-- 数据获取框 -->
          <rect class="process-box" x="50" y="75" width="120" height="50" rx="10"/>
          <text class="process-text" x="110" y="105">数据获取</text>
          <!-- 箭头1 -->
          <polygon class="process-arrow" points="180,100 200,90 200,110"/>
          <line x1="170" y1="100" x2="200" y2="100" stroke="#2c6da0" stroke-width="2"/>
          
          <!-- 数据清洗框 -->
          <rect class="process-box" x="210" y="75" width="120" height="50" rx="10"/>
          <text class="process-text" x="270" y="105">数据清洗</text>
          <!-- 箭头2 -->
          <polygon class="process-arrow" points="340,100 360,90 360,110"/>
          <line x1="330" y1="100" x2="360" y2="100" stroke="#2c6da0" stroke-width="2"/>
          
          <!-- 数据转换框 -->
          <rect class="process-box" x="370" y="75" width="120" height="50" rx="10"/>
          <text class="process-text" x="430" y="105">数据转换</text>
          <!-- 箭头3 -->
          <polygon class="process-arrow" points="500,100 520,90 520,110"/>
          <line x1="490" y1="100" x2="520" y2="100" stroke="#2c6da0" stroke-width="2"/>
          
          <!-- 数据可视化框 -->
          <rect class="process-box" x="530" y="75" width="120" height="50" rx="10"/>
          <text class="process-text" x="590" y="105">数据可视化</text>
          <!-- 箭头4 -->
          <polygon class="process-arrow" points="660,100 680,90 680,110"/>
          <line x1="650" y1="100" x2="680" y2="100" stroke="#2c6da0" stroke-width="2"/>
          
          <!-- 结果解释框 -->
          <rect class="process-box" x="690" y="75" width="120" height="50" rx="10"/>
          <text class="process-text" x="750" y="105">结果解释</text>
        </svg>
      </div>
    ')
  })
  
  output$dplyr_map <- renderUI({
    HTML(dplyr_knowledge_map())
  })
  
  output$tidyr_map <- renderUI({
    HTML(tidyr_knowledge_map())
  })
  
  output$ggplot2_map <- renderUI({
    HTML(ggplot2_knowledge_map())
  })
  
  output$rubric_svg <- renderUI({
    HTML('
      <div class="text-center">
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 300" width="80%" height="300">
          <style>
            .rubric-box { fill: #f8f9fa; stroke: #3c8dbc; stroke-width: 2; }
            .rubric-header { fill: #3c8dbc; }
            .rubric-text { font-family: Arial; font-size: 12px; }
            .rubric-header-text { fill: white; font-family: Arial; font-size: 14px; font-weight: bold; text-anchor: middle; }
            .rubric-percent { font-family: Arial; font-size: 14px; font-weight: bold; fill: #3c8dbc; text-anchor: middle; }
          </style>
          <!-- 表格标题行 -->
          <rect class="rubric-header" x="50" y="50" width="300" height="30"/>
          <text class="rubric-header-text" x="200" y="70">作业评分标准</text>
          
          <!-- 数据清洗行 -->
          <rect class="rubric-box" x="50" y="80" width="200" height="30"/>
          <text class="rubric-text" x="60" y="100">数据清洗与整理</text>
          <rect class="rubric-box" x="250" y="80" width="100" height="30"/>
          <text class="rubric-percent" x="300" y="100">25%</text>
          
          <!-- 数据转换行 -->
          <rect class="rubric-box" x="50" y="110" width="200" height="30"/>
          <text class="rubric-text" x="60" y="130">数据转换与汇总</text>
          <rect class="rubric-box" x="250" y="110" width="100" height="30"/>
          <text class="rubric-percent" x="300" y="130">25%</text>
          
          <!-- 数据可视化行 -->
          <rect class="rubric-box" x="50" y="140" width="200" height="30"/>
          <text class="rubric-text" x="60" y="160">数据可视化</text>
          <rect class="rubric-box" x="250" y="140" width="100" height="30"/>
          <text class="rubric-percent" x="300" y="160">30%</text>
          
          <!-- 分析与解释行 -->
          <rect class="rubric-box" x="50" y="170" width="200" height="30"/>
          <text class="rubric-text" x="60" y="190">分析与解释</text>
          <rect class="rubric-box" x="250" y="170" width="100" height="30"/>
          <text class="rubric-percent" x="300" y="190">20%</text>
          
          <!-- 总体结构行 -->
          <rect class="rubric-box" x="50" y="200" width="200" height="30"/>
          <text class="rubric-text" x="60" y="220">报告格式与组织</text>
          <rect class="rubric-box" x="250" y="200" width="100" height="30"/>
          <text class="rubric-percent" x="300" y="220">10%</text>
        </svg>
      </div>
    ')
  })
  
  # 企鹅数据任务提示
  output$penguin_task1_hint <- renderText({
    '# 提示：
# 使用is.na()函数检查缺失值
# 使用summarise_all(~sum(is.na(.)))统计每个变量的缺失值数量
# 使用na.omit()或filter()去除包含缺失值的行
# 使用str()检查数据结构'
  })
  
  output$penguin_task2_hint <- renderText({
    '# 提示：
# 使用mutate()创建新变量
# 使用group_by()和summarise()函数进行分组统计
# 可以使用n()函数计算样本数'
  })
  
  output$penguin_task3_hint <- renderText({
    '# 提示：
# 使用ggplot2创建可视化
# 散点图：geom_point() + geom_smooth()
# 箱线图：geom_boxplot()
# 直方图：geom_histogram() + facet_wrap()'
  })
  
  # 学生成绩任务提示
  output$student_task1_hint <- renderText({
    '# 提示：
# 使用pivot_longer()函数将宽格式转换为长格式
# 使用case_when()或cut()创建分类变量
# 使用separate()函数拆分学期列'
  })
  
  output$student_task2_hint <- renderText({
    '# 提示：
# 使用group_by()和summarise()函数进行分组统计
# 可以使用mean(), median(), max(), min()等函数
# 使用count()和mutate()计算百分比'
  })
  
  output$student_task3_hint <- renderText({
    '# 提示：
# 条形图：geom_col() 或 geom_bar(stat="identity")
# 热图：geom_tile() + scale_fill_gradient()
# 折线图：geom_line() + geom_point()'
  })
  
  # 监听企鹅数据任务按钮
  observeEvent(input$penguin_task1_check, {
    # 尝试执行用户代码
    tryCatch({
      # 创建环境并执行代码
      env <- new.env()
      env$penguins <- penguins_data()
      result <- eval(parse(text = input$penguin_task1_code), env)
      
      # 显示结果
      output$penguin_task1_result <- renderPrint({
        if (is.data.frame(result)) {
          head(result)
        } else {
          result
        }
      })
    }, error = function(e) {
      output$penguin_task1_result <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$penguin_task2_check, {
    tryCatch({
      env <- new.env()
      env$penguins <- penguins_data()
      result <- eval(parse(text = input$penguin_task2_code), env)
      
      output$penguin_task2_result <- renderPrint({
        if (is.data.frame(result)) {
          head(result)
        } else {
          result
        }
      })
    }, error = function(e) {
      output$penguin_task2_result <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$penguin_task3_1_check, {
    tryCatch({
      env <- new.env()
      env$penguins <- penguins_data()
      env$ggplot <- ggplot
      env$aes <- aes
      plot <- eval(parse(text = input$penguin_task3_code), env)
      
      output$penguin_plot1 <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$penguin_plot1 <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$penguin_task3_2_check, {
    tryCatch({
      env <- new.env()
      env$penguins <- penguins_data()
      env$ggplot <- ggplot
      env$aes <- aes
      plot <- eval(parse(text = input$penguin_task3_code2), env)
      
      output$penguin_plot2 <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$penguin_plot2 <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$penguin_task3_3_check, {
    tryCatch({
      env <- new.env()
      env$penguins <- penguins_data()
      env$ggplot <- ggplot
      env$aes <- aes
      plot <- eval(parse(text = input$penguin_task3_code3), env)
      
      output$penguin_plot3 <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$penguin_plot3 <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  # 监听学生成绩任务按钮
  observeEvent(input$student_task1_check, {
    tryCatch({
      env <- new.env()
      env$student_data <- student_data()
      result <- eval(parse(text = input$student_task1_code), env)
      
      output$student_task1_result <- renderPrint({
        if (is.data.frame(result)) {
          head(result)
        } else {
          result
        }
      })
    }, error = function(e) {
      output$student_task1_result <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$student_task2_check, {
    tryCatch({
      env <- new.env()
      env$student_data <- student_data()
      result <- eval(parse(text = input$student_task2_code), env)
      
      output$student_task2_result <- renderPrint({
        if (is.data.frame(result)) {
          head(result)
        } else {
          result
        }
      })
    }, error = function(e) {
      output$student_task2_result <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$student_task3_1_check, {
    tryCatch({
      env <- new.env()
      env$student_data <- student_data()
      env$ggplot <- ggplot
      env$aes <- aes
      plot <- eval(parse(text = input$student_task3_code), env)
      
      output$student_plot1 <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$student_plot1 <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$student_task3_2_check, {
    tryCatch({
      env <- new.env()
      env$student_data <- student_data()
      env$ggplot <- ggplot
      env$aes <- aes
      plot <- eval(parse(text = input$student_task3_code2), env)
      
      output$student_plot2 <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$student_plot2 <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
  
  observeEvent(input$student_task3_3_check, {
    tryCatch({
      env <- new.env()
      env$student_data <- student_data()
      env$ggplot <- ggplot
      env$aes <- aes
      plot <- eval(parse(text = input$student_task3_code3), env)
      
      output$student_plot3 <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$student_plot3 <- renderText({
        paste("代码执行错误:", e$message)
      })
    })
  })
}

# 运行应用
shinyApp(ui, server) 