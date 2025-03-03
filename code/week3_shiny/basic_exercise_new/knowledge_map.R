# 知识图谱生成函数

# dplyr知识图谱SVG
dplyr_knowledge_map <- function() {
  HTML('
    <div class="text-center">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 400" width="100%" height="380">
        <style>
          .node { fill: #3c8dbc; stroke: #2c6da0; stroke-width: 2; rx: 15; ry: 15; }
          .node-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; dominant-baseline: middle; }
          .edge { stroke: #2c6da0; stroke-width: 2; marker-end: url(#arrow); }
          .edge-text { fill: #2c6da0; font-family: Arial; font-size: 12px; text-anchor: middle; }
          .main-node { fill: #00a65a; stroke: #008d4c; stroke-width: 2; }
          .title-text { font-family: Arial; font-size: 18px; font-weight: bold; text-anchor: middle; fill: #3c8dbc; }
        </style>
        <!-- 定义箭头 -->
        <defs>
          <marker id="arrow" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="6" markerHeight="6" orient="auto">
            <path d="M 0 0 L 10 5 L 0 10 z" fill="#2c6da0"/>
          </marker>
        </defs>
        <!-- 标题 -->
        <text class="title-text" x="400" y="30">dplyr包核心函数知识图谱</text>
        
        <!-- 核心节点 - dplyr -->
        <rect class="node main-node" x="350" y="80" width="100" height="50"/>
        <text class="node-text" x="400" y="105">dplyr</text>
        
        <!-- 核心函数节点 -->
        <!-- select -->
        <rect class="node" x="100" y="180" width="100" height="50"/>
        <text class="node-text" x="150" y="205">select()</text>
        <line class="edge" x1="350" y1="105" x2="200" y2="180"/>
        <text class="edge-text" x="255" y="135">选择列</text>
        
        <!-- filter -->
        <rect class="node" x="250" y="180" width="100" height="50"/>
        <text class="node-text" x="300" y="205">filter()</text>
        <line class="edge" x1="380" y1="130" x2="320" y2="180"/>
        <text class="edge-text" x="330" y="160">筛选行</text>
        
        <!-- mutate -->
        <rect class="node" x="400" y="180" width="100" height="50"/>
        <text class="node-text" x="450" y="205">mutate()</text>
        <line class="edge" x1="420" y1="130" x2="440" y2="180"/>
        <text class="edge-text" x="460" y="155">创建新变量</text>
        
        <!-- summarise -->
        <rect class="node" x="550" y="180" width="100" height="50"/>
        <text class="node-text" x="600" y="205">summarise()</text>
        <line class="edge" x1="450" y1="105" x2="550" y2="180"/>
        <text class="edge-text" x="520" y="135">汇总数据</text>
        
        <!-- arrange -->
        <rect class="node" x="50" y="280" width="100" height="50"/>
        <text class="node-text" x="100" y="305">arrange()</text>
        <line class="edge" x1="150" y1="230" x2="125" y2="280"/>
        <text class="edge-text" x="110" y="255">排序</text>
        
        <!-- group_by -->
        <rect class="node" x="200" y="280" width="100" height="50"/>
        <text class="node-text" x="250" y="305">group_by()</text>
        <line class="edge" x1="300" y1="230" x2="275" y2="280"/>
        <text class="edge-text" x="260" y="255">分组</text>
        
        <!-- join -->
        <rect class="node" x="350" y="280" width="100" height="50"/>
        <text class="node-text" x="400" y="305">join家族</text>
        <line class="edge" x1="450" y1="230" x2="425" y2="280"/>
        <text class="edge-text" x="410" y="255">合并表</text>
        
        <!-- bind -->
        <rect class="node" x="500" y="280" width="100" height="50"/>
        <text class="node-text" x="550" y="305">bind家族</text>
        <line class="edge" x1="600" y1="230" x2="575" y2="280"/>
        <text class="edge-text" x="560" y="255">绑定行/列</text>
        
        <!-- 连接关系 -->
        <line class="edge" x1="250" y1="305" x2="350" y2="305"/>
        <line class="edge" x1="550" y1="230" x2="600" y2="280"/>
        <rect class="node" x="650" y="280" width="100" height="50"/>
        <text class="node-text" x="700" y="305">across()</text>
        <line class="edge" x1="650" y1="205" x2="670" y2="280"/>
        <text class="edge-text" x="680" y="240">跨列操作</text>
      </svg>
    </div>
  ')
}

# tidyr知识图谱SVG
tidyr_knowledge_map <- function() {
  HTML('
    <div class="text-center">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 400" width="100%" height="380">
        <style>
          .node { fill: #f39c12; stroke: #e08e0b; stroke-width: 2; rx: 15; ry: 15; }
          .node-text { fill: white; font-family: Arial; font-size: 14px; text-anchor: middle; dominant-baseline: middle; }
          .edge { stroke: #e08e0b; stroke-width: 2; marker-end: url(#arrow); }
          .edge-text { fill: #e08e0b; font-family: Arial; font-size: 12px; text-anchor: middle; }
          .main-node { fill: #00a65a; stroke: #008d4c; stroke-width: 2; }
          .title-text { font-family: Arial; font-size: 18px; font-weight: bold; text-anchor: middle; fill: #f39c12; }
          .concept-box { fill: #f5f5f5; stroke: #e08e0b; stroke-width: 1; stroke-dasharray: 5,3; rx: 8; ry: 8; }
          .concept-text { fill: #e08e0b; font-family: Arial; font-size: 12px; text-anchor: middle; }
        </style>
        <!-- 定义箭头 -->
        <defs>
          <marker id="arrow" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="6" markerHeight="6" orient="auto">
            <path d="M 0 0 L 10 5 L 0 10 z" fill="#e08e0b"/>
          </marker>
        </defs>
        <!-- 标题 -->
        <text class="title-text" x="400" y="30">tidyr包核心函数知识图谱</text>
        
        <!-- 核心节点 - tidyr -->
        <rect class="node main-node" x="350" y="80" width="100" height="50"/>
        <text class="node-text" x="400" y="105">tidyr</text>
        
        <!-- 左侧 - 长宽转换 -->
        <rect class="concept-box" x="80" y="160" width="280" height="200"/>
        <text class="concept-text" x="220" y="175">长宽格式转换</text>
        
        <!-- pivot_longer -->
        <rect class="node" x="120" y="200" width="120" height="50"/>
        <text class="node-text" x="180" y="225">pivot_longer()</text>
        <line class="edge" x1="350" y1="105" x2="240" y2="200"/>
        <text class="edge-text" x="275" y="145">宽转长</text>
        
        <!-- pivot_wider -->
        <rect class="node" x="120" y="280" width="120" height="50"/>
        <text class="node-text" x="180" y="305">pivot_wider()</text>
        <line class="edge" x1="180" y1="250" x2="180" y2="280"/>
        <text class="edge-text" x="200" y="265">长转宽</text>
        
        <!-- 右侧 - 列操作 -->
        <rect class="concept-box" x="440" y="160" width="280" height="200"/>
        <text class="concept-text" x="580" y="175">列操作</text>
        
        <!-- separate -->
        <rect class="node" x="480" y="200" width="120" height="50"/>
        <text class="node-text" x="540" y="225">separate()</text>
        <line class="edge" x1="450" y1="105" x2="540" y2="200"/>
        <text class="edge-text" x="510" y="145">拆分列</text>
        
        <!-- unite -->
        <rect class="node" x="480" y="280" width="120" height="50"/>
        <text class="node-text" x="540" y="305">unite()</text>
        <line class="edge" x1="540" y1="250" x2="540" y2="280"/>
        <text class="edge-text" x="560" y="265">合并列</text>
        
        <!-- 其他函数 -->
        <rect class="node" x="640" y="230" width="100" height="50"/>
        <text class="node-text" x="690" y="255">fill/drop_na</text>
        <line class="edge" x1="450" y1="85" x2="640" y2="230"/>
        <text class="edge-text" x="550" y="160">处理缺失值</text>
      </svg>
    </div>
  ')
}

# ggplot2知识图谱SVG
ggplot2_knowledge_map <- function() {
  HTML('
    <div class="text-center">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 500" width="100%" height="480">
        <style>
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
        </style>
        <!-- 定义箭头 -->
        <defs>
          <marker id="arrow" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="6" markerHeight="6" orient="auto">
            <path d="M 0 0 L 10 5 L 0 10 z" fill="#d33724"/>
          </marker>
        </defs>
        <!-- 标题 -->
        <text class="title-text" x="400" y="30">ggplot2包图层语法知识图谱</text>
        
        <!-- 核心节点 - ggplot2 -->
        <rect class="node main-node" x="350" y="70" width="100" height="50"/>
        <text class="node-text" x="400" y="95">ggplot2</text>
        
        <!-- 数据层 -->
        <rect class="layer-box" x="50" y="150" width="700" height="60"/>
        <text class="layer-text" x="100" y="170">数据层</text>
        <rect class="node" x="200" y="155" width="120" height="50"/>
        <text class="node-text" x="260" y="180">ggplot()</text>
        <line class="edge" x1="365" y1="120" x2="300" y2="155"/>
        
        <!-- 映射层 -->
        <rect class="layer-box" x="50" y="220" width="700" height="60"/>
        <text class="layer-text" x="100" y="240">映射层</text>
        <rect class="node" x="200" y="225" width="120" height="50"/>
        <text class="node-text" x="260" y="250">aes()</text>
        <line class="edge" x1="260" y1="205" x2="260" y2="225"/>
        
        <!-- 几何图形层 -->
        <rect class="layer-box" x="50" y="290" width="700" height="100"/>
        <text class="layer-text" x="100" y="310">几何图形层</text>
        
        <!-- 图表类型盒子 -->
        <rect class="chart-type" x="150" y="320" width="500" height="60"/>
        <!-- 图形类型 -->
        <text class="chart-text" x="180" y="340">geom_point()</text>
        <text class="chart-text" x="260" y="340">geom_line()</text>
        <text class="chart-text" x="340" y="340">geom_bar()</text>
        <text class="chart-text" x="420" y="340">geom_boxplot()</text>
        <text class="chart-text" x="510" y="340">geom_histogram()</text>
        <text class="chart-text" x="600" y="340">geom_...()</text>
        <text class="chart-text" x="180" y="360">散点图</text>
        <text class="chart-text" x="260" y="360">线图</text>
        <text class="chart-text" x="340" y="360">条形图</text>
        <text class="chart-text" x="420" y="360">箱线图</text>
        <text class="chart-text" x="510" y="360">直方图</text>
        <text class="chart-text" x="600" y="360">更多图形</text>
        
        <line class="edge" x1="260" y1="275" x2="260" y2="320"/>
        
        <!-- 刻面层 -->
        <rect class="layer-box" x="50" y="400" width="340" height="60"/>
        <text class="layer-text" x="100" y="420">刻面层</text>
        <rect class="node" x="200" y="405" width="120" height="50"/>
        <text class="node-text" x="260" y="430">facet_*()</text>
        <line class="edge" x1="260" y1="390" x2="260" y2="405"/>
      </svg>
    </div>
  ')
}

# 数据分析流程SVG
data_analysis_flow <- function() {
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
} 