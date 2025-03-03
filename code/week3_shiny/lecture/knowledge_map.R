# 知识图谱生成函数

# dplyr知识图谱SVG
dplyr_knowledge_map <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 800 400",
    width = "100%",
    height = "380",
    tags$style("
      .node { fill: #3c8dbc; stroke: #2c6da0; stroke-width: 2; rx: 15; ry: 15; }
      .node-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; dominant-baseline: middle; }
      .edge { stroke: #2c6da0; stroke-width: 2; marker-end: url(#arrow); }
      .edge-text { fill: #2c6da0; font-family: Arial; font-size: 12px; text-anchor: middle; }
      .main-node { fill: #00a65a; stroke: #008d4c; stroke-width: 2; }
      .title-text { font-family: Arial; font-size: 18px; font-weight: bold; text-anchor: middle; fill: #3c8dbc; }
    "),
    # 定义箭头
    tags$defs(
      tags$marker(
        id = "arrow",
        viewBox = "0 0 10 10",
        refX = "10",
        refY = "5",
        markerWidth = "6",
        markerHeight = "6",
        orient = "auto",
        tags$path(d = "M 0 0 L 10 5 L 0 10 z", fill = "#2c6da0")
      )
    ),
    # 标题
    tags$text(class = "title-text", x = "400", y = "30", "dplyr包核心函数知识图谱"),
    
    # 核心节点 - dplyr
    tags$rect(class = "node main-node", x = "350", y = "80", width = "100", height = "50"),
    tags$text(class = "node-text", x = "400", y = "105", "dplyr"),
    
    # 核心函数节点
    # select
    tags$rect(class = "node", x = "100", y = "180", width = "100", height = "50"),
    tags$text(class = "node-text", x = "150", y = "205", "select()"),
    tags$line(class = "edge", x1 = "350", y1 = "105", x2 = "200", y2 = "180"),
    tags$text(class = "edge-text", x = "255", y = "135", "选择列"),
    
    # filter
    tags$rect(class = "node", x = "250", y = "180", width = "100", height = "50"),
    tags$text(class = "node-text", x = "300", y = "205", "filter()"),
    tags$line(class = "edge", x1 = "380", y1 = "130", x2 = "320", y2 = "180"),
    tags$text(class = "edge-text", x = "330", y = "160", "筛选行"),
    
    # mutate
    tags$rect(class = "node", x = "400", y = "180", width = "100", height = "50"),
    tags$text(class = "node-text", x = "450", y = "205", "mutate()"),
    tags$line(class = "edge", x1 = "420", y1 = "130", x2 = "440", y2 = "180"),
    tags$text(class = "edge-text", x = "460", y = "155", "创建新变量"),
    
    # summarise
    tags$rect(class = "node", x = "550", y = "180", width = "100", height = "50"),
    tags$text(class = "node-text", x = "600", y = "205", "summarise()"),
    tags$line(class = "edge", x1 = "450", y1 = "105", x2 = "550", y2 = "180"),
    tags$text(class = "edge-text", x = "520", y = "135", "汇总数据"),
    
    # arrange
    tags$rect(class = "node", x = "50", y = "280", width = "100", height = "50"),
    tags$text(class = "node-text", x = "100", y = "305", "arrange()"),
    tags$line(class = "edge", x1 = "150", y1 = "230", x2 = "125", y2 = "280"),
    tags$text(class = "edge-text", x = "110", y = "255", "排序"),
    
    # group_by
    tags$rect(class = "node", x = "200", y = "280", width = "100", height = "50"),
    tags$text(class = "node-text", x = "250", y = "305", "group_by()"),
    tags$line(class = "edge", x1 = "300", y1 = "230", x2 = "275", y2 = "280"),
    tags$text(class = "edge-text", x = "260", y = "255", "分组"),
    
    # join
    tags$rect(class = "node", x = "350", y = "280", width = "100", height = "50"),
    tags$text(class = "node-text", x = "400", y = "305", "join家族"),
    tags$line(class = "edge", x1 = "450", y1 = "230", x2 = "425", y2 = "280"),
    tags$text(class = "edge-text", x = "410", y = "255", "合并表"),
    
    # bind
    tags$rect(class = "node", x = "500", y = "280", width = "100", height = "50"),
    tags$text(class = "node-text", x = "550", y = "305", "bind家族"),
    tags$line(class = "edge", x1 = "600", y1 = "230", x2 = "575", y2 = "280"),
    tags$text(class = "edge-text", x = "560", y = "255", "绑定行/列"),
    
    # 连接关系
    tags$line(class = "edge", x1 = "250", y1 = "305", x2 = "350", y2 = "305"),
    tags$line(class = "edge", x1 = "550", y1 = "230", x2 = "600", y2 = "280"),
    tags$rect(class = "node", x = "650", y = "280", width = "100", height = "50"),
    tags$text(class = "node-text", x = "700", y = "305", "across()"),
    tags$line(class = "edge", x1 = "650", y1 = "205", x2 = "670", y2 = "280"),
    tags$text(class = "edge-text", x = "680", y = "240", "跨列操作")
  )
}

# tidyr知识图谱SVG
tidyr_knowledge_map <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 800 400",
    width = "100%",
    height = "380",
    tags$style("
      .node { fill: #f39c12; stroke: #e08e0b; stroke-width: 2; rx: 15; ry: 15; }
      .node-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; dominant-baseline: middle; }
      .edge { stroke: #e08e0b; stroke-width: 2; marker-end: url(#arrow); }
      .edge-text { fill: #e08e0b; font-family: Arial; font-size: 12px; text-anchor: middle; }
      .main-node { fill: #00a65a; stroke: #008d4c; stroke-width: 2; }
      .title-text { font-family: Arial; font-size: 18px; font-weight: bold; text-anchor: middle; fill: #f39c12; }
      .concept-box { fill: #f5f5f5; stroke: #e08e0b; stroke-width: 1; stroke-dasharray: 5,3; rx: 8; ry: 8; }
      .concept-text { fill: #e08e0b; font-family: Arial; font-size: 12px; text-anchor: middle; }
    "),
    # 定义箭头
    tags$defs(
      tags$marker(
        id = "arrow",
        viewBox = "0 0 10 10",
        refX = "10",
        refY = "5",
        markerWidth = "6",
        markerHeight = "6",
        orient = "auto",
        tags$path(d = "M 0 0 L 10 5 L 0 10 z", fill = "#e08e0b")
      )
    ),
    # 标题
    tags$text(class = "title-text", x = "400", y = "30", "tidyr包核心函数知识图谱"),
    
    # 核心节点 - tidyr
    tags$rect(class = "node main-node", x = "350", y = "80", width = "100", height = "50"),
    tags$text(class = "node-text", x = "400", y = "105", "tidyr"),
    
    # 左侧 - 长宽转换
    tags$rect(class = "concept-box", x = "80", y = "160", width = "280", height = "200"),
    tags$text(class = "concept-text", x = "220", y = "175", "长宽格式转换"),
    
    # pivot_longer
    tags$rect(class = "node", x = "120", y = "200", width = "120", height = "50"),
    tags$text(class = "node-text", x = "180", y = "225", "pivot_longer()"),
    tags$line(class = "edge", x1 = "350", y1 = "105", x2 = "240", y2 = "200"),
    tags$text(class = "edge-text", x = "275", y = "145", "宽转长"),
    
    # pivot_wider
    tags$rect(class = "node", x = "120", y = "280", width = "120", height = "50"),
    tags$text(class = "node-text", x = "180", y = "305", "pivot_wider()"),
    tags$line(class = "edge", x1 = "180", y1 = "250", x2 = "180", y2 = "280"),
    tags$text(class = "edge-text", x = "200", y = "265", "长转宽"),
    
    # 右侧 - 列操作
    tags$rect(class = "concept-box", x = "440", y = "160", width = "280", height = "200"),
    tags$text(class = "concept-text", x = "580", y = "175", "列操作"),
    
    # separate
    tags$rect(class = "node", x = "480", y = "200", width = "120", height = "50"),
    tags$text(class = "node-text", x = "540", y = "225", "separate()"),
    tags$line(class = "edge", x1 = "450", y1 = "105", x2 = "540", y2 = "200"),
    tags$text(class = "edge-text", x = "510", y = "145", "拆分列"),
    
    # unite
    tags$rect(class = "node", x = "480", y = "280", width = "120", height = "50"),
    tags$text(class = "node-text", x = "540", y = "305", "unite()"),
    tags$line(class = "edge", x1 = "540", y1 = "250", x2 = "540", y2 = "280"),
    tags$text(class = "edge-text", x = "560", y = "265", "合并列"),
    
    # 其他函数
    tags$rect(class = "node", x = "640", y = "230", width = "100", height = "50"),
    tags$text(class = "node-text", x = "690", y = "255", "fill/drop_na"),
    tags$line(class = "edge", x1 = "450", y1 = "85", x2 = "640", y2 = "230"),
    tags$text(class = "edge-text", x = "550", y = "160", "处理缺失值")
  )
}

# ggplot2知识图谱SVG
ggplot2_knowledge_map <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 800 500",
    width = "100%",
    height = "480",
    tags$style("
      .node { fill: #dd4b39; stroke: #d33724; stroke-width: 2; rx: 15; ry: 15; }
      .node-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; dominant-baseline: middle; }
      .edge { stroke: #d33724; stroke-width: 2; marker-end: url(#arrow); }
      .edge-text { fill: #d33724; font-family: Arial; font-size: 12px; text-anchor: middle; }
      .main-node { fill: #00a65a; stroke: #008d4c; stroke-width: 2; }
      .title-text { font-family: Arial; font-size: 18px; font-weight: bold; text-anchor: middle; fill: #dd4b39; }
      .layer-box { fill: #f5f5f5; stroke: #d33724; stroke-width: 1; rx: 8; ry: 8; }
      .layer-text { fill: #dd4b39; font-family: Arial; font-size: 12px; text-anchor: middle; }
      .chart-type { fill: #f5f5f5; stroke: #d33724; stroke-width: 1; stroke-dasharray: 5,3; rx: 8; ry: 8; }
      .chart-text { fill: #777; font-family: Arial; font-size: 11px; }
    "),
    # 定义箭头
    tags$defs(
      tags$marker(
        id = "arrow",
        viewBox = "0 0 10 10",
        refX = "10",
        refY = "5",
        markerWidth = "6",
        markerHeight = "6",
        orient = "auto",
        tags$path(d = "M 0 0 L 10 5 L 0 10 z", fill = "#d33724")
      )
    ),
    # 标题
    tags$text(class = "title-text", x = "400", y = "30", "ggplot2包图层语法知识图谱"),
    
    # 核心节点 - ggplot2
    tags$rect(class = "node main-node", x = "350", y = "70", width = "100", height = "50"),
    tags$text(class = "node-text", x = "400", y = "95", "ggplot2"),
    
    # 数据层
    tags$rect(class = "layer-box", x = "50", y = "150", width = "700", height = "60"),
    tags$text(class = "layer-text", x = "100", y = "170", "数据层"),
    tags$rect(class = "node", x = "200", y = "155", width = "120", height = "50"),
    tags$text(class = "node-text", x = "260", y = "180", "ggplot()"),
    tags$line(class = "edge", x1 = "365", y1 = "120", x2 = "300", y2 = "155"),
    
    # 映射层
    tags$rect(class = "layer-box", x = "50", y = "220", width = "700", height = "60"),
    tags$text(class = "layer-text", x = "100", y = "240", "映射层"),
    tags$rect(class = "node", x = "200", y = "225", width = "120", height = "50"),
    tags$text(class = "node-text", x = "260", y = "250", "aes()"),
    tags$line(class = "edge", x1 = "260", y1 = "205", x2 = "260", y2 = "225"),
    
    # 几何图形层
    tags$rect(class = "layer-box", x = "50", y = "290", width = "700", height = "100"),
    tags$text(class = "layer-text", x = "100", y = "310", "几何图形层"),
    
    # 图表类型盒子
    tags$rect(class = "chart-type", x = "150", y = "320", width = "500", height = "60"),
    # 图形类型
    tags$text(class = "chart-text", x = "180", y = "340", "geom_point()"),
    tags$text(class = "chart-text", x = "260", y = "340", "geom_line()"),
    tags$text(class = "chart-text", x = "340", y = "340", "geom_bar()"),
    tags$text(class = "chart-text", x = "420", y = "340", "geom_boxplot()"),
    tags$text(class = "chart-text", x = "510", y = "340", "geom_histogram()"),
    tags$text(class = "chart-text", x = "600", y = "340", "geom_...()"),
    tags$text(class = "chart-text", x = "180", y = "360", "散点图"),
    tags$text(class = "chart-text", x = "260", y = "360", "线图"),
    tags$text(class = "chart-text", x = "340", y = "360", "条形图"),
    tags$text(class = "chart-text", x = "420", y = "360", "箱线图"),
    tags$text(class = "chart-text", x = "510", y = "360", "直方图"),
    tags$text(class = "chart-text", x = "600", y = "360", "更多图形"),
    
    tags$line(class = "edge", x1 = "260", y1 = "275", x2 = "260", y2 = "320"),
    
    # 刻面层
    tags$rect(class = "layer-box", x = "50", y = "400", width = "340", height = "60"),
    tags$text(class = "layer-text", x = "100", y = "420", "刻面层"),
    tags$rect(class = "node", x = "200", y = "405", width = "120", height = "50"),
    tags$text(class = "node-text", x = "260", y = "430", "facet_*()"),
    tags$line(class = "edge", x1 = "260", y1 = "380", x2 = "260", y2 = "405"),
    
    # 主题层
    tags$rect(class = "layer-box", x = "410", y = "400", width = "340", height = "60"),
    tags$text(class = "layer-text", x = "460", y = "420", "主题层"),
    tags$rect(class = "node", x = "560", y = "405", width = "120", height = "50"),
    tags$text(class = "node-text", x = "620", y = "430", "theme()"),
    tags$line(class = "edge", x1 = "450", y1 = "120", x2 = "590", y2 = "405"),
    
    # 添加元素
    tags$rect(class = "node", x = "560", y = "155", width = "120", height = "50"),
    tags$text(class = "node-text", x = "620", y = "180", "labs()"),
    tags$line(class = "edge", x1 = "435", y1 = "120", x2 = "560", y2 = "155"),
    tags$text(class = "edge-text", x = "510", y = "130", "标签和标题"),
    
    tags$rect(class = "node", x = "560", y = "225", width = "120", height = "50"),
    tags$text(class = "node-text", x = "620", y = "250", "scales"),
    tags$line(class = "edge", x1 = "620", y1 = "205", x2 = "620", y2 = "225"),
    tags$text(class = "edge-text", x = "650", y = "215", "坐标轴设置")
  )
}

# 数据分析流程图SVG
data_analysis_flow <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 800 250",
    width = "100%",
    height = "220",
    tags$style("
      .step-box { fill: #3c8dbc; stroke: #2c6da0; stroke-width: 2; rx: 15; ry: 15; }
      .step-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; dominant-baseline: middle; }
      .step-arrow { fill: #2c6da0; }
      .step-line { stroke: #2c6da0; stroke-width: 2; }
      .step-desc { fill: #666; font-family: Arial; font-size: 12px; text-anchor: middle; }
      .title-text { font-family: Arial; font-size: 18px; font-weight: bold; text-anchor: middle; fill: #3c8dbc; }
    "),
    # 标题
    tags$text(class = "title-text", x = "400", y = "30", "数据分析流程"),
    
    # 步骤1：数据获取
    tags$rect(class = "step-box", x = "50", y = "70", width = "120", height = "60"),
    tags$text(class = "step-text", x = "110", y = "100", "数据获取"),
    tags$text(class = "step-desc", x = "110", y = "150", "读取数据文件, API"),
    
    # 步骤2：数据清洗
    tags$rect(class = "step-box", x = "210", y = "70", width = "120", height = "60"),
    tags$text(class = "step-text", x = "270", y = "100", "数据清洗"),
    tags$text(class = "step-desc", x = "270", y = "150", "处理缺失值、异常值"),
    tags$line(class = "step-line", x1 = "170", y1 = "100", x2 = "203", y2 = "100"),
    tags$polygon(points = "200,100 210,96 210,104", fill = "#2c6da0"),
    
    # 步骤3：数据转换
    tags$rect(class = "step-box", x = "370", y = "70", width = "120", height = "60"),
    tags$text(class = "step-text", x = "430", y = "100", "数据转换"),
    tags$text(class = "step-desc", x = "430", y = "150", "变量变换、格式转换"),
    tags$line(class = "step-line", x1 = "330", y1 = "100", x2 = "363", y2 = "100"),
    tags$polygon(points = "360,100 370,96 370,104", fill = "#2c6da0"),
    
    # 步骤4：数据可视化
    tags$rect(class = "step-box", x = "530", y = "70", width = "120", height = "60"),
    tags$text(class = "step-text", x = "590", y = "100", "数据可视化"),
    tags$text(class = "step-desc", x = "590", y = "150", "图表生成、探索性分析"),
    tags$line(class = "step-line", x1 = "490", y1 = "100", x2 = "523", y2 = "100"),
    tags$polygon(points = "520,100 530,96 530,104", fill = "#2c6da0"),
    
    # 步骤5：结果解释
    tags$rect(class = "step-box", x = "690", y = "70", width = "120", height = "60"),
    tags$text(class = "step-text", x = "750", y = "100", "结果解释"),
    tags$text(class = "step-desc", x = "750", y = "150", "统计推断、报告撰写"),
    tags$line(class = "step-line", x1 = "650", y1 = "100", x2 = "683", y2 = "100"),
    tags$polygon(points = "680,100 690,96 690,104", fill = "#2c6da0"),
    
    # 循环指示
    tags$path(
      d = "M 750 180 Q 750 200 700 200 L 150 200 Q 110 200 110 180",
      fill = "none",
      stroke = "#2c6da0",
      "stroke-width" = "2",
      "stroke-dasharray" = "5,3"
    ),
    tags$polygon(points = "110,184 106,174 114,174", fill = "#2c6da0"),
    tags$text(class = "step-desc", x = "400", y = "220", "迭代过程：基于分析结果调整前面的步骤")
  )
} 