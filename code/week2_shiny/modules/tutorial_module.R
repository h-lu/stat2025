# 帮助和教程模块
# 此模块包含学习资料、知识点总结和实践练习

tutorialUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        tabBox(
          title = "学习资料与练习", width = NULL,
          
          # 知识点总结面板
          tabPanel("知识点总结",
            fluidRow(
              column(12,
                box(
                  title = "数据导入与理解知识点", width = NULL, status = "primary",
                  
                  # 数据导入
                  h4("1. 数据导入常用函数"),
                  tags$ul(
                    tags$li(strong("CSV格式："), code("read.csv()"), "和", code("readr::read_csv()")),
                    tags$li(strong("Excel格式："), code("readxl::read_excel()")),
                    tags$li(strong("R数据格式："), code("readRDS()"), "和", code("load()")),
                    tags$li(strong("其他格式："), code("read.table()"), "、", code("read.delim()"), "等")
                  ),
                  
                  # 数据结构和类型
                  h4("2. 数据结构和类型"),
                  tags$ul(
                    tags$li(strong("数据类型："), "numeric, character, logical, factor, Date等"),
                    tags$li(strong("数据结构："), "vector, list, matrix, data.frame, tibble等"),
                    tags$li(strong("数据结构查看："), code("str()"), "、", code("glimpse()"), "、", code("head()"), "、", code("summary()"))
                  ),
                  
                  # 数据质量问题
                  h4("3. 数据质量问题"),
                  tags$ul(
                    tags$li(strong("缺失值："), "使用", code("is.na()"), "检测，", code("na.omit()"), "或", code("na.rm=TRUE"), "处理"),
                    tags$li(strong("异常值："), "使用箱线图、", code("boxplot.stats()$out"), "或", code("quantile()"), "检测"),
                    tags$li(strong("不一致值："), "需要根据业务规则检查和处理"),
                    tags$li(strong("数据类型问题："), "使用", code("as.numeric()"), "、", code("as.character()"), "等转换函数")
                  ),
                  
                  # 描述性统计
                  h4("4. 描述性统计"),
                  tags$ul(
                    tags$li(strong("集中趋势："), code("mean()"), "、", code("median()"), "、", code("mode()")),
                    tags$li(strong("离散程度："), code("sd()"), "、", code("var()"), "、", code("IQR()"), "、", code("range()")),
                    tags$li(strong("分布特征："), code("quantile()"), "、", code("hist()"), "、", code("boxplot()")),
                    tags$li(strong("关联关系："), code("cor()"), "、", code("cov()"), "、", code("table()"))
                  ),
                  
                  # dplyr函数
                  h4("5. dplyr基本函数"),
                  tags$ul(
                    tags$li(code("select()"), "：选择列"),
                    tags$li(code("filter()"), "：筛选行"),
                    tags$li(code("arrange()"), "：排序"),
                    tags$li(code("mutate()"), "：创建或修改列"),
                    tags$li(code("summarise()"), "：汇总计算"),
                    tags$li(code("group_by()"), "：分组"),
                    tags$li(code("join"), "函数：连接数据框")
                  )
                )
              )
            )
          ),
          
          # 练习题面板
          tabPanel("练习题",
            fluidRow(
              column(12,
                h3("实践练习题"),
                tabBox(
                  width = NULL,
                  
                  # 基础练习
                  tabPanel("基础练习",
                    box(
                      title = "数据导入和查看", width = NULL, status = "info",
                      tags$ol(
                        tags$li('在"数据导入"模块中，分别使用三种不同格式导入示例数据，观察导入过程和代码有什么不同？'),
                        tags$li("对比readr::read_csv()和base R的read.csv()函数，它们在使用和性能上有什么区别？"),
                        tags$li('使用"数据查看"模块，分析示例数据中的缺失值，哪些列缺失值较多？可能的原因是什么？')
                      )
                    ),
                    
                    box(
                      title = "描述性统计和dplyr操作", width = NULL, status = "success",
                      tags$ol(
                        tags$li('使用"数据分析"模块中的描述性统计功能，分析学生成绩数据中各科成绩的分布特征'),
                        tags$li('使用dplyr的filter功能，筛选出电影数据中评分大于8的电影，这些电影有什么共同特点？'),
                        tags$li('使用dplyr的group_by和summarise功能，计算销售数据中各产品类别的平均销售额')
                      )
                    )
                  ),
                  
                  # 进阶练习
                  tabPanel("进阶练习",
                    box(
                      title = "数据质量处理", width = NULL, status = "warning",
                      tags$ol(
                        tags$li("在R中，写出处理学生成绩数据中缺失值的完整代码（可使用多种策略）"),
                        tags$li("为电影数据创建一个新变量，表示电影是否为'高评分高票房'（评分和票房均高于平均值）"),
                        tags$li("分析销售数据中的异常值，并写出检测和处理这些异常值的R代码")
                      )
                    ),
                    
                    box(
                      title = "综合数据分析", width = NULL, status = "danger",
                      tags$ol(
                        tags$li("使用dplyr函数链，对学生成绩数据进行完整的分析流程：数据清洗、转换、分组汇总"),
                        tags$li('分析电影数据中"评分"与"票房"的关系，包括相关性分析和可视化'),
                        tags$li("对销售数据进行时间趋势分析，识别销售高峰和低谷，并探索可能的原因")
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # 延伸阅读面板
          tabPanel("延伸阅读",
            fluidRow(
              column(12,
                box(
                  title = "推荐学习资源", width = NULL, status = "primary",
                  h4("官方文档和教程"),
                  tags$ul(
                    tags$li(a("R数据导入/导出官方文档", href = "https://cran.r-project.org/doc/manuals/r-release/R-data.html", target = "_blank")),
                    tags$li(a("tidyverse学习资源", href = "https://www.tidyverse.org/learn/", target = "_blank")),
                    tags$li(a("R for Data Science (中文版)", href = "https://r4ds.had.co.nz/", target = "_blank"))
                  ),
                  
                  h4("进阶学习主题"),
                  tags$ul(
                    tags$li("数据清洗的高级技术和最佳实践"),
                    tags$li("大数据集的高效处理方法"),
                    tags$li("使用tidyr进行数据重塑和整理"),
                    tags$li("数据可视化最佳实践和高级技巧")
                  ),
                  
                  h4("实用R包推荐"),
                  tags$ul(
                    tags$li(code("readr"), "：快速高效的数据导入"),
                    tags$li(code("readxl"), "：Excel文件导入"),
                    tags$li(code("haven"), "：SPSS、SAS和Stata文件导入"),
                    tags$li(code("janitor"), "：数据清洗辅助函数"),
                    tags$li(code("skimr"), "：数据摘要和EDA工具"),
                    tags$li(code("naniar"), "：缺失值分析和可视化")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

tutorialServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 本模块主要是展示静态内容，暂不需要服务器端逻辑
  })
} 