import streamlit as st
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression
import plotly.express as px
import plotly.graph_objects as go
from utils.data_utils import generate_advertising_data
from utils.plot_utils import plot_simple_regression, plot_residuals

def app():
    """简单线性回归模型页面"""
    
    st.header("简单线性回归模型")
    
    # 模型介绍部分
    with st.expander("模型形式与参数", expanded=True):
        col1, col2 = st.columns([3, 2])
        
        with col1:
            st.markdown("""
            <div class="important-box">
            <h4>简单线性回归模型形式</h4>
            
            简单线性回归模型可以表示为：
            
            $$y = \\beta_0 + \\beta_1 x + \\epsilon$$
            
            其中：
            <ul>
                <li>$y$ 是因变量</li>
                <li>$x$ 是自变量</li>
                <li>$\\beta_0$ 是截距，表示当$x=0$时，$y$的期望值</li>
                <li>$\\beta_1$ 是斜率，表示自变量$x$每增加一个单位，因变量$y$的平均变化量</li>
                <li>$\\epsilon$ 是随机误差项，反映模型无法解释的随机变异</li>
            </ul>
            </div>
            
            <div class="note-box">
            <h4>简单线性回归的假设</h4>
            <ol>
                <li><b>线性性</b>：因变量与自变量之间存在线性关系</li>
                <li><b>独立性</b>：误差项之间相互独立</li>
                <li><b>同方差性</b>：误差项的方差为常数（同质性）</li>
                <li><b>正态性</b>：误差项服从均值为0的正态分布</li>
            </ol>
            </div>
            """, unsafe_allow_html=True)
            
            # 回归参数解释
            st.markdown("""
            <div class="warning-box">
            <h4>回归系数的解释</h4>
            <ul>
                <li><b>截距 $\\beta_0$</b>：当自变量$x$为0时，因变量$y$的预测值（在实际情况中，自变量为0可能没有现实意义）</li>
                <li><b>斜率 $\\beta_1$</b>：自变量$x$每增加一个单位，因变量$y$的预测值平均增加$\\beta_1$个单位（边际效应）</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 简单的交互式演示
            st.markdown("#### 调整参数，观察回归线变化")
            
            beta0 = st.slider("截距 (β₀)", -10.0, 10.0, 2.0, 0.1)
            beta1 = st.slider("斜率 (β₁)", -2.0, 2.0, 0.5, 0.1)
            
            # 生成演示数据
            x_demo = np.linspace(0, 10, 100)
            y_true = beta0 + beta1 * x_demo
            y_demo = y_true + np.random.normal(0, 1, 100)
            
            # 可视化
            demo_data = pd.DataFrame({"x": x_demo, "y": y_demo})
            fig = px.scatter(demo_data, x="x", y="y", title="简单线性回归示例")
            
            # 添加真实回归线
            fig.add_shape(
                type="line",
                x0=min(x_demo),
                y0=beta0 + beta1 * min(x_demo),
                x1=max(x_demo),
                y1=beta0 + beta1 * max(x_demo),
                line=dict(color="red", width=2)
            )
            
            fig.update_layout(
                annotations=[
                    dict(
                        x=5,
                        y=beta0 + beta1 * 5 + 2,
                        text=f"y = {beta0:.1f} + {beta1:.1f}x",
                        showarrow=False,
                        font=dict(size=14)
                    )
                ]
            )
            
            st.plotly_chart(fig, use_container_width=True)
            
            # 商业例子
            st.markdown("""
            **商业例子**: 
            
            如果$x$表示广告支出（万元），$y$表示销售额（万元），且$\\beta_0 = 10$，$\\beta_1 = 2$，则回归方程为：
            
            $y = 10 + 2x$
            
            这表示：
            - 当不投入广告时，预期销售额为10万元
            - 每增加1万元广告支出，销售额平均增加2万元
            """)
    
    # 最小二乘法部分
    with st.expander("最小二乘法", expanded=True):
        col1, col2 = st.columns([1, 1])
        
        with col1:
            st.markdown("""
            <div class="important-box">
            <h4>最小二乘法原理</h4>
            <p>最小二乘法(OLS)是一种估计回归系数的方法，其目标是找到能使误差平方和最小的回归线。</p>
            
            <p>给定数据点$(x_1, y_1), (x_2, y_2), ..., (x_n, y_n)$，我们希望找到参数$\\beta_0$和$\\beta_1$，使残差平方和(RSS)最小化：</p>
            
            $$\\text{RSS} = \\sum_{i=1}^{n} (y_i - \\hat{y}_i)^2 = \\sum_{i=1}^{n} (y_i - (\\hat{\\beta}_0 + \\hat{\\beta}_1 x_i))^2$$
            
            <p>其中$\\hat{\\beta}_0$和$\\hat{\\beta}_1$是$\\beta_0$和$\\beta_1$的估计值，$\\hat{y}_i$是因变量$y_i$的预测值。</p>
            </div>
            
            <div class="note-box">
            <h4>最小二乘估计的公式</h4>
            <p>对RSS关于$\\beta_0$和$\\beta_1$求导并令其等于0，可以得到：</p>
            
            $$\\hat{\\beta}_1 = \\frac{\\sum_{i=1}^{n}(x_i - \\bar{x})(y_i - \\bar{y})}{\\sum_{i=1}^{n}(x_i - \\bar{x})^2}$$
            
            $$\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x}$$
            
            <p>其中$\\bar{x}$和$\\bar{y}$分别是$x$和$y$的样本均值。</p>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 交互式OLS演示
            st.markdown("#### 交互式最小二乘法演示")
            
            # 生成少量点以便清晰展示
            np.random.seed(42)
            n_points = st.slider("数据点数量", 5, 20, 10)
            noise_level = st.slider("噪声程度", 0.1, 3.0, 1.0, 0.1)
            
            x_ols = np.random.uniform(0, 10, n_points)
            y_ols = 2 + 0.5 * x_ols + np.random.normal(0, noise_level, n_points)
            
            # 计算OLS估计
            model_ols = LinearRegression()
            model_ols.fit(x_ols.reshape(-1, 1), y_ols)
            
            beta0_est = model_ols.intercept_
            beta1_est = model_ols.coef_[0]
            
            # 可视化
            fig_ols = go.Figure()
            
            # 添加数据点
            fig_ols.add_trace(go.Scatter(
                x=x_ols,
                y=y_ols,
                mode='markers',
                name='数据点'
            ))
            
            # 添加OLS回归线
            x_line = np.linspace(0, 10, 100)
            y_line = beta0_est + beta1_est * x_line
            
            fig_ols.add_trace(go.Scatter(
                x=x_line,
                y=y_line,
                mode='lines',
                name=f'OLS回归线: y = {beta0_est:.2f} + {beta1_est:.2f}x'
            ))
            
            # 添加残差线
            for i in range(n_points):
                y_pred = beta0_est + beta1_est * x_ols[i]
                fig_ols.add_shape(
                    type="line",
                    x0=x_ols[i],
                    y0=y_ols[i],
                    x1=x_ols[i],
                    y1=y_pred,
                    line=dict(color="red", width=1, dash="dot"),
                )
            
            fig_ols.update_layout(
                title="最小二乘法拟合效果",
                xaxis_title="x",
                yaxis_title="y",
                showlegend=True
            )
            
            st.plotly_chart(fig_ols, use_container_width=True)
            
            # RSS可视化
            if st.checkbox("查看RSS与参数关系"):
                # 创建不同截距和斜率的组合
                beta0_range = np.linspace(beta0_est - 2, beta0_est + 2, 50)
                beta1_range = np.linspace(beta1_est - 1, beta1_est + 1, 50)
                beta0_grid, beta1_grid = np.meshgrid(beta0_range, beta1_range)
                
                # 计算每个组合的RSS
                rss = np.zeros_like(beta0_grid)
                for i in range(len(beta0_range)):
                    for j in range(len(beta1_range)):
                        b0, b1 = beta0_grid[j, i], beta1_grid[j, i]
                        y_pred = b0 + b1 * x_ols
                        rss[j, i] = np.sum((y_ols - y_pred) ** 2)
                
                # 创建3D曲面图
                fig_rss = go.Figure(data=[go.Surface(z=rss, x=beta0_range, y=beta1_range)])
                fig_rss.update_layout(
                    title="RSS与回归参数的关系",
                    scene=dict(
                        xaxis_title="截距 (β₀)",
                        yaxis_title="斜率 (β₁)",
                        zaxis_title="残差平方和 (RSS)"
                    ),
                    width=600,
                    height=500
                )
                
                # 标记最小RSS点
                fig_rss.add_trace(go.Scatter3d(
                    x=[beta0_est],
                    y=[beta1_est],
                    z=[np.min(rss)],
                    mode='markers',
                    marker=dict(size=5, color='red'),
                    name='最小值'
                ))
                
                st.plotly_chart(fig_rss, use_container_width=True)
    
    # 模型评估指标部分
    with st.expander("模型评估指标", expanded=False):
        col1, col2 = st.columns([2, 1])
        
        with col1:
            st.markdown("""
            <div class="important-box">
            <h4>拟合优度 (R²)</h4>
            <p>R²是衡量模型拟合程度的指标，表示模型解释的因变量方差比例。</p>
            
            $$R^2 = 1 - \\frac{SSE}{SST} = 1 - \\frac{\\sum(y_i - \\hat{y}_i)^2}{\\sum(y_i - \\bar{y})^2}$$
            
            <p>其中：</p>
            <ul>
                <li>SSE：残差平方和，表示模型未解释的变异</li>
                <li>SST：总平方和，表示因变量的总变异</li>
            </ul>
            
            <p>R²取值范围为[0,1]：</p>
            <ul>
                <li>R²=1：模型完美拟合数据</li>
                <li>R²=0：模型不比直接使用均值更好</li>
                <li>通常R²越接近1，表示模型拟合越好</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
            
            st.markdown("""
            <div class="note-box">
            <h4>残差标准误差(RSE)</h4>
            <p>残差标准误差衡量因变量实际值偏离预测值的平均程度，单位与因变量相同。</p>
            
            $$RSE = \\sqrt{\\frac{SSE}{n-2}} = \\sqrt{\\frac{\\sum(y_i - \\hat{y}_i)^2}{n-2}}$$
            
            <p>其中n是样本量，2表示简单线性回归模型中的两个参数($\\beta_0$和$\\beta_1$)。</p>
            
            <p>RSE越小，表示模型拟合越好。可以用来估计预测区间。</p>
            </div>
            """, unsafe_allow_html=True)
            
            st.markdown("""
            <div class="warning-box">
            <h4>均方误差(MSE)与均方根误差(RMSE)</h4>
            <p>MSE是残差平方的平均值，RMSE是MSE的平方根。</p>
            
            $$MSE = \\frac{\\sum(y_i - \\hat{y}_i)^2}{n}$$
            
            $$RMSE = \\sqrt{MSE} = \\sqrt{\\frac{\\sum(y_i - \\hat{y}_i)^2}{n}}$$
            
            <p>RMSE的单位与因变量相同，可以直观理解为平均预测误差。</p>
            </div>
            """, unsafe_allow_html=True)
            
            st.markdown("""
            <div class="note-box">
            <h4>系数显著性检验</h4>
            <p>使用t检验评估回归系数是否显著不为0：</p>
            
            $$t = \\frac{\\hat{\\beta}_j}{SE(\\hat{\\beta}_j)}$$
            
            <p>其中$SE(\\hat{\\beta}_j)$是系数估计值的标准误差。</p>
            
            <p>比较|t|与临界值(通常基于5%显著性水平)，或直接查看p值：</p>
            <ul>
                <li>p < 0.05: 系数统计显著</li>
                <li>p ≥ 0.05: 无法拒绝系数为0的原假设</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 交互式演示R²含义
            st.markdown("#### R²的直观解释")
            
            # 生成数据
            np.random.seed(123)
            x_r2 = np.random.uniform(0, 10, 50)
            
            # 控制R²的滑块
            corr_strength = st.slider("调整相关强度", 0.0, 1.0, 0.7, 0.1)
            
            # 生成不同相关性的数据
            noise_level = np.sqrt(1/corr_strength - 1) if corr_strength > 0 else 10
            y_r2 = 2 + 0.5 * x_r2 + np.random.normal(0, noise_level, 50)
            
            # 计算R²
            model_r2 = LinearRegression()
            model_r2.fit(x_r2.reshape(-1, 1), y_r2)
            r2_value = model_r2.score(x_r2.reshape(-1, 1), y_r2)
            
            # 散点图和回归线
            r2_data = pd.DataFrame({"x": x_r2, "y": y_r2})
            fig_r2 = px.scatter(r2_data, x="x", y="y", title=f"R² = {r2_value:.3f}")
            
            # 添加回归线
            x_line = np.linspace(0, 10, 100)
            y_line = model_r2.intercept_ + model_r2.coef_[0] * x_line
            fig_r2.add_trace(px.line(x=x_line, y=y_line).data[0])
            
            st.plotly_chart(fig_r2, use_container_width=True)
            
            # 商业解释
            st.markdown(f"""
            #### 商业解释
            
            这个例子中，R² = {r2_value:.3f}意味着：
            
            - 模型解释了{r2_value*100:.1f}%的销售额变异
            - 剩余{(1-r2_value)*100:.1f}%由模型未包含的因素或随机性解释
            
            **对决策的影响**：
            - R²高：可以更有信心地基于模型做出预测和决策
            - R²低：需要考虑其他未纳入模型的重要因素
            """)
            
            # 计算MSE和RMSE
            y_pred_r2 = model_r2.predict(x_r2.reshape(-1, 1))
            mse_value = np.mean((y_r2 - y_pred_r2) ** 2)
            rmse_value = np.sqrt(mse_value)
            
            st.metric("MSE", f"{mse_value:.3f}")
            st.metric("RMSE", f"{rmse_value:.3f}")
            
            # p值计算(简化版)
            n = len(x_r2)
            x_mean = np.mean(x_r2)
            ssr = np.sum((x_r2 - x_mean) ** 2)
            s_beta1 = rmse_value / np.sqrt(ssr)
            t_value = model_r2.coef_[0] / s_beta1
            
            st.metric("t值 (β₁)", f"{t_value:.3f}")
            
            if abs(t_value) > 1.96:
                st.success("斜率系数在5%水平下显著")
            else:
                st.error("斜率系数在5%水平下不显著")
    
    # 实际案例分析
    with st.expander("实际案例分析", expanded=True):
        st.subheader("案例：广告支出与销售额关系分析")
        
        # 生成广告数据
        case_data = generate_advertising_data(n=100, seed=42)
        
        # 展示数据头
        st.markdown("#### 数据预览")
        st.dataframe(case_data.head())
        
        # 各变量的描述性统计
        st.markdown("#### 描述性统计")
        st.dataframe(case_data.describe().round(2))
        
        # 相关性热图
        st.markdown("#### 变量间相关性")
        
        # 计算相关系数
        corr = case_data.corr().round(3)
        
        # 使用Plotly创建热图
        fig_corr = go.Figure(data=go.Heatmap(
                    z=corr.values,
                    x=corr.columns,
                    y=corr.columns,
                    colorscale='Viridis',
                    text=corr.values,
                    texttemplate='%{text}',
                    textfont={"size":10}))
        
        fig_corr.update_layout(
            title="变量相关系数矩阵",
            height=400
        )
        
        st.plotly_chart(fig_corr, use_container_width=True)
        
        # 不同广告渠道与销售额的关系
        st.markdown("#### 各广告渠道与销售额关系")
        
        channels = ["TV", "Radio", "Newspaper"]
        
        # 构建各个渠道的回归模型
        models = {}
        metrics = {}
        
        for channel in channels:
            X = case_data[channel].values.reshape(-1, 1)
            y = case_data["Sales"].values
            
            model = LinearRegression()
            model.fit(X, y)
            
            y_pred = model.predict(X)
            r2 = model.score(X, y)
            mse = np.mean((y - y_pred) ** 2)
            rmse = np.sqrt(mse)
            
            models[channel] = model
            metrics[channel] = {
                "截距": model.intercept_,
                "系数": model.coef_[0],
                "R²": r2,
                "RMSE": rmse
            }
        
        # 创建比较表格
        metrics_df = pd.DataFrame(metrics).T
        metrics_df.columns = ["截距 (β₀)", "系数 (β₁)", "R²", "RMSE"]
        metrics_df = metrics_df.round(4)
        
        st.dataframe(metrics_df)
        
        # 可视化表示
        col1, col2 = st.columns(2)
        
        with col1:
            # 回归系数柱状图
            coef_df = pd.DataFrame({
                "广告渠道": channels,
                "边际效应": [metrics[channel]["系数"] for channel in channels]
            })
            
            fig_coef = px.bar(
                coef_df, 
                x="广告渠道", 
                y="边际效应",
                title="各广告渠道的边际效应(每单位投入增加的销售额)",
                color="边际效应",
                text="边际效应",
                color_continuous_scale="Viridis"
            )
            
            fig_coef.update_traces(texttemplate='%{text:.3f}', textposition='outside')
            st.plotly_chart(fig_coef, use_container_width=True)
        
        with col2:
            # R²比较柱状图
            r2_df = pd.DataFrame({
                "广告渠道": channels,
                "R²": [metrics[channel]["R²"] for channel in channels]
            })
            
            fig_r2 = px.bar(
                r2_df, 
                x="广告渠道", 
                y="R²",
                title="各广告渠道回归模型的R²",
                color="R²",
                text="R²",
                color_continuous_scale="Viridis"
            )
            
            fig_r2.update_traces(texttemplate='%{text:.3f}', textposition='outside')
            st.plotly_chart(fig_r2, use_container_width=True)
        
        # 案例解释与业务建议
        st.markdown("""
        #### 案例分析结论
        
        基于上述分析，我们可以得出以下结论：
        
        1. **不同广告渠道的效果不同**：
           - 广播广告(Radio)的边际效应最高，每增加1万元投入，销售额平均增加约0.19万元
           - 电视广告(TV)次之，每增加1万元投入，销售额平均增加约0.05万元
           - 报纸广告(Newspaper)的边际效应最低
        
        2. **解释力差异**：
           - 电视广告模型的R²最高，能解释约61%的销售额变异
           - 广播广告解释力次之
           - 报纸广告模型解释力最弱
        
        #### 业务建议
        
        1. **预算分配**：
           - 基于边际效应，增加广播广告的投入可能带来最高的短期销售增长
           - 但也要考虑到电视广告的高解释力，意味着其效果可能更稳定可预测
        
        2. **策略优化**：
           - 考虑减少报纸广告的投入，或重新设计报纸广告策略以提高其效果
           - 测试不同的电视和广播广告组合，找到最优投入比例
        
        3. **下一步分析**：
           - 考虑广告渠道间可能存在的交互效应（多元回归分析）
           - 分析不同时间段、不同市场的广告效果差异
        """)
        
        # 实际应用示例
        st.markdown("#### 应用示例：预算优化")
        
        # 创建简单的预算优化工具
        st.markdown("假设您有100万元的广告预算，使用下面的滑块分配到不同渠道:")
        
        total_budget = 100.0
        
        col1, col2, col3 = st.columns(3)
        
        with col1:
            tv_budget = st.slider("电视广告预算(万元)", 0.0, total_budget, 50.0, 5.0)
        
        budget_left = total_budget - tv_budget
        
        with col2:
            radio_budget = st.slider("广播广告预算(万元)", 0.0, budget_left, min(30.0, budget_left), 5.0)
        
        with col3:
            newspaper_budget = total_budget - tv_budget - radio_budget
            st.metric("报纸广告预算(万元)", f"{newspaper_budget:.1f}")
        
        # 预测销售额
        baseline_sales = 5.0  # 假设基础销售额
        predicted_sales = (
            baseline_sales + 
            metrics["TV"]["系数"] * tv_budget + 
            metrics["Radio"]["系数"] * radio_budget + 
            metrics["Newspaper"]["系数"] * newspaper_budget
        )
        
        # 显示预测结果
        st.success(f"预计总销售额: {predicted_sales:.2f}万元")
        
        # 可视化预算分配和销售贡献
        budget_data = pd.DataFrame({
            "渠道": ["电视", "广播", "报纸"],
            "预算": [tv_budget, radio_budget, newspaper_budget],
            "销售贡献": [
                metrics["TV"]["系数"] * tv_budget,
                metrics["Radio"]["系数"] * radio_budget,
                metrics["Newspaper"]["系数"] * newspaper_budget
            ]
        })
        
        # 预算分配饼图
        fig_budget = px.pie(
            budget_data, 
            values="预算", 
            names="渠道", 
            title="广告预算分配",
            color="渠道",
            color_discrete_sequence=px.colors.sequential.Viridis
        )
        
        # 销售贡献条形图
        fig_contrib = px.bar(
            budget_data, 
            x="渠道", 
            y="销售贡献",
            title="各渠道对销售的贡献",
            color="渠道",
            text="销售贡献",
            color_discrete_sequence=px.colors.sequential.Viridis
        )
        
        fig_contrib.update_traces(texttemplate='%{text:.2f}', textposition='outside')
        
        col1, col2 = st.columns(2)
        with col1:
            st.plotly_chart(fig_budget, use_container_width=True)
        with col2:
            st.plotly_chart(fig_contrib, use_container_width=True)
    
    # 交互式回归分析工具
    st.subheader("交互式回归分析工具")
    
    # 数据选择
    data_option = st.selectbox("选择数据", ["广告投入与销售额", "自定义数据"])
    
    if data_option == "广告投入与销售额":
        data = generate_advertising_data(n=100)
        
        # 选择自变量
        x_var = st.selectbox("选择自变量", ["TV", "Radio", "Newspaper"])
        y_var = "Sales"  # 固定因变量为Sales
        
        # 提取数据
        X = data[x_var].values
        y = data[y_var].values
    else:
        # 自定义数据上传
        uploaded_file = st.file_uploader("上传CSV数据文件", type=["csv"])
        
        if uploaded_file is not None:
            data = pd.read_csv(uploaded_file)
            st.write("数据预览：")
            st.dataframe(data.head())
            
            # 选择变量
            x_var = st.selectbox("选择自变量", data.columns)
            y_var = st.selectbox("选择因变量", data.columns)
            
            # 提取数据
            X = data[x_var].values
            y = data[y_var].values
        else:
            st.info("请上传数据文件或选择示例数据")
            st.stop()
    
    # 显示散点图和回归线
    st.subheader(f"{x_var}与{y_var}的关系")
    fig = plot_simple_regression(X, y, x_var, y_var, f"{x_var}与{y_var}的线性关系")
    st.plotly_chart(fig, use_container_width=True)
    
    # 执行回归分析
    model = LinearRegression()
    model.fit(X.reshape(-1, 1), y)
    
    # 显示回归结果
    st.subheader("回归结果")
    col1, col2 = st.columns(2)
    with col1:
        st.metric("截距 (β₀)", f"{model.intercept_:.4f}")
        st.metric("斜率 (β₁)", f"{model.coef_[0]:.4f}")
    
    with col2:
        # 计算R²
        y_pred = model.predict(X.reshape(-1, 1))
        r2 = model.score(X.reshape(-1, 1), y)
        mse = np.mean((y - y_pred) ** 2)
        
        st.metric("R²", f"{r2:.4f}")
        st.metric("均方误差 (MSE)", f"{mse:.4f}")
    
    # 回归方程
    st.markdown(f"**回归方程**: $y = {model.intercept_:.4f} + {model.coef_[0]:.4f} \\times {x_var}$")
    
    # 残差分析
    st.subheader("残差分析")
    residual_fig = plot_residuals(model, X.reshape(-1, 1), y)
    st.plotly_chart(residual_fig, use_container_width=True)
    
    # 预测工具
    st.subheader("预测工具")
    pred_value = st.number_input(f"输入{x_var}的值", min_value=float(X.min()), max_value=float(X.max()))
    
    if st.button("预测"):
        prediction = model.predict([[pred_value]])[0]
        conf_interval = 1.96 * np.sqrt(mse)  # 简单的95%置信区间
        
        st.success(f"预测的{y_var}值: {prediction:.2f}")
        st.info(f"95%置信区间: [{prediction - conf_interval:.2f}, {prediction + conf_interval:.2f}]")
        
        # 在图上标注预测点
        pred_fig = px.scatter(x=X, y=y, labels={'x': x_var, 'y': y_var})
        
        # 添加回归线
        x_range = np.linspace(X.min(), X.max(), 100)
        y_pred_line = model.predict(x_range.reshape(-1, 1))
        pred_fig.add_trace(px.line(x=x_range, y=y_pred_line).data[0])
        
        # 添加预测点
        pred_fig.add_trace(px.scatter(x=[pred_value], y=[prediction], 
                                     color_discrete_sequence=['red']).data[0])
        
        # 添加置信区间
        pred_fig.add_trace(px.line(x=[pred_value, pred_value], 
                                  y=[prediction - conf_interval, prediction + conf_interval],
                                  color_discrete_sequence=['green']).data[0])
        
        pred_fig.update_layout(title=f"{x_var}={pred_value}的预测结果")
        st.plotly_chart(pred_fig, use_container_width=True)

if __name__ == "__main__":
    app() 