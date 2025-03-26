import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
from utils.data_utils import generate_advertising_data

def app():
    """回归分析概述页面"""
    
    st.header("回归分析概述")
    
    # 回归分析的目的
    with st.expander("🎯 回归分析的目的", expanded=True):
        col1, col2 = st.columns([3, 2])
        
        with col1:
            st.markdown("""
            <div class="note-box">
            <h4>回归分析的主要目的</h4>
            <ul>
                <li><b>研究关系</b>：分析因变量与一个或多个自变量之间的关系。</li>
                <li><b>建立模型</b>：构建模型以进行预测和解释现象。</li>
                <li><b>评估关联</b>：评估变量之间关联的程度和方向。</li>
            </ul>
            <p>回归分析是商业分析中常用的核心技术之一，可以解答以下商业问题：</p>
            <ul>
                <li>哪些因素关键影响销售额？各自的影响程度如何？</li>
                <li>如何通过产品特征预测产品价格？</li>
                <li>不同的营销投入对业绩的边际贡献是多少？</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 示例数据可视化
            data = generate_advertising_data(n=50)
            fig = px.scatter(data, x="TV", y="Sales", title="电视广告与销售额关系示例")
            fig.update_layout(height=300)
            st.plotly_chart(fig, use_container_width=True)
    
    # 回归分析的应用场景
    with st.expander("🌐 回归分析的应用场景", expanded=True):
        st.markdown("""
        <div class="warning-box">
        <h4>回归分析的应用场景</h4>
        <ul>
            <li><b>预测</b>：销售额预测、房价预测、股票价格预测等。</li>
            <li><b>行为分析</b>：分析影响消费者购买行为的因素。</li>
            <li><b>效果评估</b>：评估营销活动、政策实施等效果。</li>
            <li><b>绩效识别</b>：识别影响企业绩效的关键指标。</li>
        </ul>
        </div>
        """, unsafe_allow_html=True)
        
        # 应用场景交互式选择
        scene = st.selectbox("选择一个应用场景查看示例：", 
                          ["销售预测", "房价预测", "广告效果评估", "消费者行为分析"])
        
        # 根据选择显示对应的示例
        if scene == "销售预测":
            st.write("销售预测示例：预测不同产品的销售量")
            # 生成示例数据
            sales_data = pd.DataFrame({
                "月份": range(1, 13),
                "季节": ["冬", "冬", "春", "春", "春", "夏", "夏", "夏", "秋", "秋", "秋", "冬"],
                "产品A销量": [120, 110, 140, 150, 160, 180, 200, 210, 170, 150, 140, 130],
                "产品B销量": [80, 75, 90, 110, 120, 140, 130, 120, 110, 100, 90, 85]
            })
            # 可视化
            fig = px.line(sales_data, x="月份", y=["产品A销量", "产品B销量"], 
                        color_discrete_sequence=["blue", "red"],
                        title="产品销量月度趋势")
            st.plotly_chart(fig, use_container_width=True)
        
        elif scene == "房价预测":
            st.write("房价预测示例：基于房屋特征预测房价")
            # 生成示例数据
            housing_data = pd.DataFrame({
                "面积(平方米)": [70, 90, 120, 150, 180, 200, 220],
                "房间数": [2, 3, 3, 4, 4, 5, 5],
                "楼龄(年)": [20, 15, 10, 5, 8, 2, 1],
                "房价(万元)": [350, 450, 600, 750, 900, 1000, 1100]
            })
            # 可视化
            fig = px.scatter(housing_data, x="面积(平方米)", y="房价(万元)", 
                           size="房间数", color="楼龄(年)",
                           title="房屋面积、房间数、楼龄与房价的关系")
            st.plotly_chart(fig, use_container_width=True)
        
        elif scene == "广告效果评估":
            st.write("广告效果评估示例：分析不同渠道的广告投入与销售额关系")
            # 使用之前生成的数据
            data = generate_advertising_data(n=100)
            # 可视化
            fig = px.scatter_3d(data, x="TV", y="Radio", z="Sales", 
                              color="Newspaper", title="多渠道广告投入与销售额关系")
            st.plotly_chart(fig, use_container_width=True)
        
        else:  # 消费者行为分析
            st.write("消费者行为分析示例：分析影响消费者购买决策的因素")
            # 生成示例数据
            behavior_data = pd.DataFrame({
                "价格敏感度": np.random.uniform(0, 1, 100),
                "促销敏感度": np.random.uniform(0, 1, 100),
                "品牌忠诚度": np.random.uniform(0, 1, 100),
                "购买频率": np.random.uniform(0, 10, 100)
            })
            # 可视化
            fig = px.scatter_matrix(behavior_data, 
                                  title="消费者行为特征关系矩阵")
            st.plotly_chart(fig, use_container_width=True)
    
    # 回归分析的类型
    with st.expander("📊 回归分析的类型", expanded=True):
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("""
            <div class="important-box">
            <h4>按关系类型</h4>
            <ul>
                <li><b>线性回归</b>：自变量与因变量之间呈线性关系。</li>
                <li><b>非线性回归</b>：自变量与因变量之间呈非线性关系。</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            st.markdown("""
            <div class="important-box">
            <h4>按自变量数量</h4>
            <ul>
                <li><b>简单线性回归</b>：仅有一个自变量。</li>
                <li><b>多元线性回归</b>：有多个自变量。</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
        
        # 添加交互式演示
        st.subheader("回归类型交互式演示")
        
        # 为演示创建示例数据
        x = np.linspace(0, 10, 100)
        noise = np.random.normal(0, 1, 100)
        
        # 线性关系
        y_linear = 2 + 0.5 * x + noise
        # 非线性关系
        y_nonlinear = 2 + 0.5 * x**2 + noise
        
        # 选择回归类型
        regression_type = st.radio("选择回归类型演示：", ["线性回归", "非线性回归"])
        
        if regression_type == "线性回归":
            # 线性回归演示
            data = pd.DataFrame({"x": x, "y": y_linear})
            fig = px.scatter(data, x="x", y="y", title="线性关系示例")
            fig.add_shape(
                type="line",
                x0=min(x),
                y0=2 + 0.5 * min(x),
                x1=max(x),
                y1=2 + 0.5 * max(x),
                line=dict(color="red", width=2)
            )
            st.plotly_chart(fig, use_container_width=True)
            st.markdown("这是一个线性关系的例子，可以写为：$y = 2 + 0.5x + \\epsilon$")
        else:
            # 非线性回归演示
            data = pd.DataFrame({"x": x, "y": y_nonlinear})
            fig = px.scatter(data, x="x", y="y", title="非线性关系示例")
            # 添加非线性曲线
            x_curve = np.linspace(min(x), max(x), 100)
            y_curve = 2 + 0.5 * x_curve**2
            fig.add_trace(px.line(x=x_curve, y=y_curve).data[0])
            st.plotly_chart(fig, use_container_width=True)
            st.markdown("这是一个非线性关系的例子，可以写为：$y = 2 + 0.5x^2 + \\epsilon$")
    
    # 回归分析与其他统计方法的区别
    with st.expander("🔄 回归分析与其他统计方法的区别", expanded=False):
        st.markdown("""
        <div class="note-box">
        <h4>回归分析与相关分析的区别</h4>
        <ul>
            <li><b>相关分析</b>：仅关注变量之间关系的强度和方向，不区分自变量和因变量。</li>
            <li><b>回归分析</b>：不仅描述变量间的关系，更侧重于建立自变量对因变量的预测模型。</li>
        </ul>
        </div>
        
        <div class="note-box">
        <h4>回归分析与方差分析的区别</h4>
        <ul>
            <li><b>方差分析</b>：主要关注分类自变量对连续因变量的影响，比较不同组间的均值差异。</li>
            <li><b>回归分析</b>：主要处理连续型自变量，分析自变量对因变量的定量影响。</li>
        </ul>
        </div>
        """, unsafe_allow_html=True)
        
        # 添加示例图表
        method_comparison = pd.DataFrame({
            "方法": ["相关分析", "回归分析", "方差分析"],
            "目的": ["测量变量间关系的强度和方向", "建立预测模型，解释自变量对因变量的影响", "比较不同组别的平均差异"],
            "输出": ["相关系数(r)", "回归系数(β)", "F统计量"],
            "典型应用": ["分析身高与体重的关系", "预测广告投入对销售的影响", "比较不同市场战略的效果差异"]
        })
        st.table(method_comparison)
    
    # 回归分析的基本步骤
    with st.expander("🔍 回归分析的基本步骤", expanded=False):
        st.markdown("""
        <div class="warning-box">
        <h4>回归分析的基本步骤</h4>
        <ol>
            <li><b>模型确定</b>：选择合适的回归模型类型 (线性/非线性，简单/多元)。</li>
            <li><b>参数估计</b>：使用样本数据估计模型中的未知参数。</li>
            <li><b>模型检验</b>：评估模型的拟合效果和统计显著性。</li>
            <li><b>模型应用</b>：运用已建立的模型进行预测、解释和决策。</li>
        </ol>
        </div>
        """, unsafe_allow_html=True)
        
        # 流程图
        st.graphviz_chart('''
        digraph {
            node [shape=box, style=filled, fillcolor=lightblue, fontname="Microsoft YaHei"];
            edge [fontname="Microsoft YaHei"];
            
            数据收集 -> 探索性分析;
            探索性分析 -> 模型确定;
            模型确定 -> 参数估计;
            参数估计 -> 模型检验;
            模型检验 -> 模型应用;
            
            模型检验 -> 模型重新确定 [label="检验不通过", color=red];
            模型重新确定 -> 参数估计;
        }
        ''')
        
        st.markdown("""
        <div class="note-box">
        <h4>商业中回归分析应用流程</h4>
        <ol>
            <li><b>明确商业问题</b>：确定需要分析的因变量和潜在相关的自变量。</li>
            <li><b>数据收集</b>：收集相关数据，可能来自公司内部数据库、市场调研等。</li>
            <li><b>数据预处理</b>：处理缺失值、异常值，可能需要进行特征工程。</li>
            <li><b>探索性分析</b>：通过可视化和描述性统计了解数据特征。</li>
            <li><b>建立模型</b>：根据问题和数据特性选择适合的回归模型。</li>
            <li><b>模型评估</b>：评估模型的预测能力和解释力。</li>
            <li><b>结果解读</b>：将统计结果转化为可执行的商业洞察。</li>
            <li><b>应用决策</b>：基于模型结果制定营销策略、定价策略等决策。</li>
        </ol>
        </div>
        """, unsafe_allow_html=True)
    
    # 添加交互式问答环节
    st.subheader("📝 知识检查")
    
    with st.form("quiz_form"):
        st.write("测试您对回归分析基础知识的理解")
        
        q1 = st.radio(
            "1. 回归分析的主要目的是什么？",
            ["仅测量变量间的相关性", "建立预测模型并解释因素影响", "比较不同组别的平均值", "评估数据的正态性"]
        )
        
        q2 = st.radio(
            "2. 简单线性回归与多元线性回归的区别在于：",
            ["模型复杂性", "自变量的数量", "自变量和因变量的关系类型", "样本量要求"]
        )
        
        q3 = st.radio(
            "3. 在商业分析中，回归分析常用于：",
            ["仅用于销售预测", "仅用于市场细分", "各种预测、因素影响分析和效果评估", "仅用于价格优化"]
        )
        
        submit = st.form_submit_button("提交答案")
        
        if submit:
            score = 0
            if q1 == "建立预测模型并解释因素影响":
                score += 1
            if q2 == "自变量的数量":
                score += 1
            if q3 == "各种预测、因素影响分析和效果评估":
                score += 1
            
            st.success(f"您的得分是：{score}/3")
            
            # 提供解释
            st.markdown("""
            **解析**：
            1. 回归分析的主要目的是建立预测模型并解释自变量对因变量的影响。
            2. 简单线性回归只有一个自变量，而多元线性回归有多个自变量。
            3. 在商业分析中，回归分析广泛应用于预测、影响因素分析和效果评估等方面。
            """)
    
    # 章节导航
    st.markdown("---")
    st.markdown("👉 **下一章**: [简单线性回归模型](#)")
    
    # 参考文献
    with st.expander("📚 参考文献"):
        st.markdown("""
        1. James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An Introduction to Statistical Learning*. Springer.
        2. Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W. (2005). *Applied Linear Statistical Models (5th ed.)*. McGraw-Hill.
        3. Fox, J. (2016). *Applied Regression Analysis and Generalized Linear Models (3rd ed.)*. SAGE Publications.
        """)

if __name__ == "__main__":
    app() 