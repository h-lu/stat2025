import streamlit as st
import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from sklearn.linear_model import LinearRegression
from utils.data_utils import generate_advertising_data, calculate_vif
from utils.plot_utils import plot_multiple_regression_scatter_matrix, plot_residuals, plot_variable_importance, plot_3d_regression

def app():
    """多元线性回归模型页面"""
    
    st.header("多元线性回归模型")
    
    # 多元回归概述
    with st.expander("多元回归概述", expanded=True):
        col1, col2 = st.columns([2, 1])
        
        with col1:
            st.markdown("""
            <div class="note-box">
            <h4>多元线性回归的意义</h4>
            <p>在商业环境中，一个因变量往往受多个因素影响。例如，销售额不仅受电视广告的影响，还可能受广播、报纸、社交媒体等多种营销渠道的影响。多元线性回归能够同时考虑多个自变量的影响，更全面地分析因素间的关系。</p>
            </div>
            
            <div class="important-box">
            <h4>多元回归的优势</h4>
            <ol>
                <li><b>更全面的解释</b>：考虑多个因素的联合影响，提供更全面的因果解释</li>
                <li><b>控制变量</b>：通过控制其他变量，分离单个变量的"净效应"</li>
                <li><b>提高预测精度</b>：利用更多信息，通常能提供更准确的预测</li>
                <li><b>识别交互作用</b>：可以分析不同变量间的交互效应</li>
            </ol>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 简单示例图
            st.image("https://miro.medium.com/max/1400/1*Vn9lfLwn9lpsKGR6INONAA.webp", 
                    caption="多元回归示意图（多个自变量影响一个因变量）")
            
            # 商业示例
            st.markdown("""
            #### 商业应用示例
            
            1. **市场营销**：分析不同广告渠道的综合效果
            2. **价格策略**：考虑产品特性、竞争和市场因素确定价格
            3. **销售预测**：结合多种指标预测销售表现
            4. **客户行为**：分析影响客户购买决策的多种因素
            """)
        
        # 简单与多元回归比较
        st.markdown("""
        <div class="warning-box">
        <h4>简单回归与多元回归的比较</h4>
        
        <table>
          <tr>
            <th>特征</th>
            <th>简单线性回归</th>
            <th>多元线性回归</th>
          </tr>
          <tr>
            <td>自变量数量</td>
            <td>一个</td>
            <td>两个或更多</td>
          </tr>
          <tr>
            <td>模型复杂度</td>
            <td>低</td>
            <td>中到高</td>
          </tr>
          <tr>
            <td>可视化</td>
            <td>二维散点图和直线</td>
            <td>多维空间，难以直观可视化</td>
          </tr>
          <tr>
            <td>系数解释</td>
            <td>直接的边际效应</td>
            <td>控制其他变量后的边际效应</td>
          </tr>
          <tr>
            <td>常见问题</td>
            <td>遗漏变量偏误</td>
            <td>多重共线性</td>
          </tr>
        </table>
        </div>
        """, unsafe_allow_html=True)
        
        # 添加交互式演示
        st.markdown("#### 遗漏变量偏误直观演示")
        
        # 生成有相关性的自变量数据
        np.random.seed(42)
        n = 100
        x1 = np.random.uniform(0, 10, n)
        x2 = 2 + 0.7 * x1 + np.random.normal(0, 2, n)  # x2与x1相关
        y = 5 + 0.3 * x1 + 0.6 * x2 + np.random.normal(0, 1, n)
        
        demo_data = pd.DataFrame({"x1": x1, "x2": x2, "y": y})
        
        # 简单回归 (仅使用x1)
        simple_model = LinearRegression()
        simple_model.fit(x1.reshape(-1, 1), y)
        
        # 多元回归 (同时使用x1和x2)
        multiple_model = LinearRegression()
        multiple_model.fit(demo_data[["x1", "x2"]], y)
        
        # 创建对比表格
        comparison_df = pd.DataFrame({
            "模型": ["简单回归 (仅x1)", "多元回归 (x1+x2)"],
            "x1系数": [simple_model.coef_[0], multiple_model.coef_[0]],
            "x2系数": ["未包含", multiple_model.coef_[1]],
            "R²": [simple_model.score(x1.reshape(-1, 1), y), 
                  multiple_model.score(demo_data[["x1", "x2"]], y)]
        })
        
        st.dataframe(comparison_df.round(3))
        
        # 解释
        st.markdown("""
        <div class="note-box">
        <p>上表展示了遗漏变量偏误的影响：</p>
        <ul>
            <li>简单回归中，x1的系数被高估，因为它"吸收"了x2的部分效应</li>
            <li>多元回归中，我们得到x1和x2的"净效应"</li>
            <li>R²显示多元模型解释了更多的因变量变异</li>
        </ul>
        <p>这在商业分析中尤为重要，例如评估某一特定营销渠道的真实效果时，需要控制其他渠道的影响。</p>
        </div>
        """, unsafe_allow_html=True)
        
        # 输出两个模型预测的图表对比
        if st.checkbox("查看两个模型预测效果对比"):
            # 预测值
            simple_pred = simple_model.predict(x1.reshape(-1, 1))
            multiple_pred = multiple_model.predict(demo_data[["x1", "x2"]])
            
            # 计算指标
            simple_mse = np.mean((y - simple_pred) ** 2)
            multiple_mse = np.mean((y - multiple_pred) ** 2)
            
            # 创建对比图表
            fig = go.Figure()
            
            # 添加原始数据点
            fig.add_trace(go.Scatter(
                x=np.arange(n),
                y=y,
                mode='markers',
                name='真实值'
            ))
            
            # 添加简单回归预测
            fig.add_trace(go.Scatter(
                x=np.arange(n),
                y=simple_pred,
                mode='lines',
                name=f'简单回归预测 (MSE: {simple_mse:.3f})'
            ))
            
            # 添加多元回归预测
            fig.add_trace(go.Scatter(
                x=np.arange(n),
                y=multiple_pred,
                mode='lines',
                name=f'多元回归预测 (MSE: {multiple_mse:.3f})'
            ))
            
            fig.update_layout(
                title="简单回归 vs 多元回归预测效果对比",
                xaxis_title="观测序号",
                yaxis_title="y值",
                legend=dict(
                    yanchor="top",
                    y=0.99,
                    xanchor="left",
                    x=0.01
                )
            )
            
            st.plotly_chart(fig, use_container_width=True)
            
            st.markdown(f"""
            多元回归模型的MSE ({multiple_mse:.3f})明显低于简单回归模型的MSE ({simple_mse:.3f})，
            表明包含更多相关变量能显著提高模型的预测准确性。
            """)
    
    # 模型形式和参数解释
    with st.expander("模型形式和参数解释", expanded=True):
        col1, col2 = st.columns([3, 2])
        
        with col1:
            st.markdown("""
            <div class="important-box">
            <h4>多元线性回归模型形式</h4>
            
            多元线性回归模型可以表示为：
            
            $$y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\dots + \\beta_p x_p + \\epsilon$$
            
            其中：
            <ul>
                <li>$y$：因变量</li>
                <li>$x_1, x_2, \\dots, x_p$：$p$个自变量</li>
                <li>$\\beta_0$：截距</li>
                <li>$\\beta_1, \\beta_2, \\dots, \\beta_p$：偏回归系数，控制其他自变量不变时，每个自变量对因变量的边际影响</li>
                <li>$\\epsilon$：随机误差项</li>
            </ul>
            </div>
            
            <div class="note-box">
            <h4>偏回归系数的解释</h4>
            <p>$\\beta_j$表示在其他自变量$x_1, \\dots, x_{j-1}, x_{j+1}, \\dots, x_p$保持不变的情况下，自变量$x_j$每增加一个单位，因变量$y$的平均变化量。</p>
            
            <p>这个解释非常重要，因为它体现了"控制变量"的思想，即我们能够分离出单个自变量的"净效应"。</p>
            
            <p>例如，在分析销售额时，$\\beta_{TV}$表示在广播和报纸广告投入保持不变的情况下，电视广告每增加一单位带来的销售额平均增长。</p>
            </div>
            """, unsafe_allow_html=True)
            
            st.markdown("""
            <div class="warning-box">
            <h4>参数估计</h4>
            <p>与简单线性回归类似，多元线性回归同样使用最小二乘法(OLS)估计参数。不同的是，计算过程需要用到矩阵运算：</p>
            
            $$\\hat{\\beta} = (X^TX)^{-1}X^Ty$$
            
            <p>其中$X$是自变量矩阵，$y$是因变量向量。这个公式是简单线性回归最小二乘公式的矩阵推广。</p>
            </div>
            """, unsafe_allow_html=True)
            
            st.markdown("""
            <div class="note-box">
            <h4>模型假设</h4>
            <p>多元线性回归的假设与简单线性回归基本相同，但有一些额外的考虑：</p>
            <ol>
                <li><b>线性性</b>：因变量与每个自变量之间存在线性关系</li>
                <li><b>独立性</b>：观测值之间相互独立</li>
                <li><b>同方差性</b>：误差项方差恒定</li>
                <li><b>正态性</b>：误差项服从正态分布</li>
                <li><b>无多重共线性</b>：自变量之间不存在高度相关性（这是多元回归特有的假设）</li>
            </ol>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 提供一些商业示例
            st.markdown("""
            #### 商业中的参数解释
            
            假设房价模型：
            
            $Price = 50000 + 100 \\times Area + 5000 \\times Rooms - 2000 \\times Age$
            
            参数解释：
            - $\\beta_0 = 50000$：基础房价
            - $\\beta_{Area} = 100$：其他因素不变时，每增加1平方米，房价平均增加100元
            - $\\beta_{Rooms} = 5000$：其他因素不变时，每增加一个房间，房价平均增加5000元
            - $\\beta_{Age} = -2000$：其他因素不变时，房龄每增加1年，房价平均降低2000元
            """)
            
            # 交互式演示
            st.markdown("#### 参数交互式演示")
            
            # 创建示例数据
            np.random.seed(123)
            n_samples = 100
            
            # 销售员绩效示例
            experience = np.random.uniform(1, 10, n_samples)  # 销售经验(年)
            training = np.random.randint(0, 5, n_samples)     # 培训次数
            calls = np.random.uniform(20, 60, n_samples)      # 每周电话数
            
            # 设置真实系数
            beta0 = 10      # 基础销售额
            beta1 = 3       # 经验系数
            beta2 = 5       # 培训系数
            beta3 = 0.2     # 电话系数
            
            # 生成销售额
            sales = beta0 + beta1 * experience + beta2 * training + beta3 * calls + np.random.normal(0, 5, n_samples)
            
            # 创建数据框
            sales_data = pd.DataFrame({
                '销售经验(年)': experience,
                '培训次数': training,
                '每周电话数': calls,
                '月销售额(万元)': sales
            })
            
            # 显示数据预览
            st.write("销售员绩效数据示例:")
            st.dataframe(sales_data.head(3))
            
            # 拟合模型
            X = sales_data[['销售经验(年)', '培训次数', '每周电话数']].values
            y = sales_data['月销售额(万元)'].values
            
            model = LinearRegression()
            model.fit(X, y)
            
            # 显示模型参数
            st.write("多元回归模型参数:")
            
            coef_df = pd.DataFrame({
                '变量': ['截距', '销售经验(年)', '培训次数', '每周电话数'],
                '系数': [model.intercept_, *model.coef_],
                '解释': [
                    '基础销售额',
                    '销售经验每增加1年，销售额平均增加的金额(万元)',
                    '参加一次培训，销售额平均增加的金额(万元)',
                    '每周多打一个电话，销售额平均增加的金额(万元)'
                ]
            })
            
            st.dataframe(coef_df)
            
            # 参数调整演示
            st.markdown("##### 调整参数观察影响")
            
            # 用户可调整的参数
            exp_value = st.slider("销售经验(年)", 1.0, 10.0, 5.0, 0.5)
            training_value = st.slider("培训次数", 0, 4, 2)
            calls_value = st.slider("每周电话数", 20, 60, 40, 5)
            
            # 预测销售额
            prediction = model.predict([[exp_value, training_value, calls_value]])[0]
            
            # 显示预测结果
            st.success(f"预测月销售额: {prediction:.2f}万元")
            
            # 计算各因素贡献
            contributions = pd.DataFrame({
                '因素': ['基础销售额', '销售经验', '培训', '电话拜访'],
                '贡献额(万元)': [
                    model.intercept_,
                    model.coef_[0] * exp_value,
                    model.coef_[1] * training_value,
                    model.coef_[2] * calls_value
                ]
            })
            
            # 可视化各因素贡献
            fig = px.bar(
                contributions, 
                x='因素', 
                y='贡献额(万元)',
                text='贡献额(万元)',
                title="各因素对销售额的贡献",
                color='贡献额(万元)',
                color_continuous_scale='RdBu'
            )
            
            fig.update_traces(texttemplate='%{text:.2f}', textposition='outside')
            
            st.plotly_chart(fig, use_container_width=True)
    
    # 多重共线性
    with st.expander("多重共线性问题", expanded=False):
        col1, col2 = st.columns([2, 1])
        
        with col1:
            st.markdown("""
            <div class="warning-box">
            <h4>多重共线性问题</h4>
            <p>多重共线性是指自变量之间存在强相关关系。这是多元回归中的一个常见问题，会导致：</p>
            <ul>
                <li>回归系数估计不稳定（方差膨胀）</li>
                <li>系数解释困难（符号可能与预期相反）</li>
                <li>难以确定单个变量的重要性</li>
            </ul>
            </div>
            
            <div class="note-box">
            <h4>多重共线性的诊断</h4>
            <p>常用的多重共线性诊断方法包括：</p>
            <ol>
                <li><b>相关系数矩阵</b>：检查自变量两两之间的相关系数，高相关可能导致多重共线性</li>
                <li><b>方差膨胀因子(VIF)</b>：测量自变量之间多重共线性的程度
                    <ul>
                        <li>VIF = 1：无多重共线性</li>
                        <li>1 < VIF < 5：轻度多重共线性</li>
                        <li>5 ≤ VIF < 10：中度多重共线性</li>
                        <li>VIF ≥ 10：严重多重共线性</li>
                    </ul>
                </li>
                <li><b>条件数</b>：设计矩阵特征值的比率，值越大表示多重共线性越严重</li>
            </ol>
            </div>
            """, unsafe_allow_html=True)
            
            st.markdown("""
            <div class="important-box">
            <h4>多重共线性的处理方法</h4>
            <p>当检测到多重共线性时，可以采用以下方法处理：</p>
            <ol>
                <li><b>删除变量</b>：移除高度相关的变量中的一个或多个</li>
                <li><b>使用主成分分析(PCA)</b>：将相关变量转换为不相关的主成分</li>
                <li><b>岭回归</b>：通过添加L2正则化项减小系数方差</li>
                <li><b>收集更多数据</b>：增加样本量有助于减轻多重共线性的影响</li>
                <li><b>变量组合</b>：将高度相关的变量组合成一个新变量</li>
            </ol>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            # 多重共线性演示
            st.markdown("#### 多重共线性直观演示")
            
            # 生成具有多重共线性的数据
            np.random.seed(42)
            n_samples = 100
            
            # 生成两个相关的自变量
            x1 = np.random.uniform(0, 10, n_samples)
            
            # 用户可调整的相关性强度
            corr_strength = st.slider("变量相关性强度", 0.0, 0.99, 0.8, 0.05, 
                                    help="调整自变量间的相关性强度，值越大表示相关性越强")
            
            # 基于相关性强度生成x2
            x2 = corr_strength * x1 + (1 - corr_strength) * np.random.uniform(0, 10, n_samples)
            
            # 计算相关系数
            correlation = np.corrcoef(x1, x2)[0, 1]
            
            # 生成因变量
            beta0, beta1, beta2 = 5, 2, 3
            y = beta0 + beta1 * x1 + beta2 * x2 + np.random.normal(0, 2, n_samples)
            
            # 创建数据框
            collin_data = pd.DataFrame({
                'x1': x1,
                'x2': x2, 
                'y': y
            })
            
            # 拟合模型
            X = collin_data[['x1', 'x2']].values
            model_collin = LinearRegression()
            model_collin.fit(X, y)
            
            # 计算VIF
            # VIF(x1) = 1 / (1 - R²(x1 ~ x2))
            from sklearn.linear_model import LinearRegression
            model_x1_x2 = LinearRegression().fit(x2.reshape(-1, 1), x1)
            r2_x1_x2 = model_x1_x2.score(x2.reshape(-1, 1), x1)
            vif_x1 = 1 / (1 - r2_x1_x2)
            
            model_x2_x1 = LinearRegression().fit(x1.reshape(-1, 1), x2)
            r2_x2_x1 = model_x2_x1.score(x1.reshape(-1, 1), x2)
            vif_x2 = 1 / (1 - r2_x2_x1)
            
            # 显示统计信息
            st.metric("变量相关系数", f"{correlation:.3f}")
            
            col1_vif, col2_vif = st.columns(2)
            with col1_vif:
                st.metric("x1的VIF", f"{vif_x1:.3f}")
            with col2_vif:
                st.metric("x2的VIF", f"{vif_x2:.3f}")
            
            # 显示回归系数
            st.write("回归系数（注意与真实值的差异）:")
            coef_df = pd.DataFrame({
                '变量': ['截距', 'x1', 'x2'],
                '真实系数': [beta0, beta1, beta2],
                '估计系数': [model_collin.intercept_, model_collin.coef_[0], model_collin.coef_[1]],
                '差异百分比': ['--', f"{(model_collin.coef_[0] - beta1)/beta1*100:.1f}%", 
                         f"{(model_collin.coef_[1] - beta2)/beta2*100:.1f}%"]
            })
            st.dataframe(coef_df)
            
            # 可视化多重共线性
            if st.checkbox("查看散点图"):
                fig = px.scatter(collin_data, x="x1", y="x2", 
                                title=f"x1与x2的散点图 (r = {correlation:.3f})")
                st.plotly_chart(fig, use_container_width=True)
            
            # 系数稳定性演示
            if st.checkbox("演示系数稳定性"):
                # 重复采样并记录系数
                n_reps = 50
                beta1_samples = []
                beta2_samples = []
                
                for i in range(n_reps):
                    # 随机抽样80%的数据
                    sample_idx = np.random.choice(n_samples, int(n_samples * 0.8), replace=False)
                    X_sample = X[sample_idx]
                    y_sample = y[sample_idx]
                    
                    # 拟合模型
                    model_sample = LinearRegression()
                    model_sample.fit(X_sample, y_sample)
                    
                    # 记录系数
                    beta1_samples.append(model_sample.coef_[0])
                    beta2_samples.append(model_sample.coef_[1])
                
                # 计算系数标准差
                beta1_std = np.std(beta1_samples)
                beta2_std = np.std(beta2_samples)
                
                # 可视化系数分布
                coef_samples = pd.DataFrame({
                    'beta1': beta1_samples,
                    'beta2': beta2_samples
                })
                
                fig = px.scatter(coef_samples, x="beta1", y="beta2", 
                                title="多重共线性下系数的不稳定性",
                                labels={"beta1": "x1的系数", "beta2": "x2的系数"})
                
                # 添加真实值点
                fig.add_trace(go.Scatter(
                    x=[beta1], y=[beta2],
                    mode='markers',
                    marker=dict(color='red', size=10),
                    name='真实系数'
                ))
                
                st.plotly_chart(fig, use_container_width=True)
                
                st.info(f"系数标准差: beta1={beta1_std:.3f}, beta2={beta2_std:.3f}")
                st.markdown("""
                在多重共线性情况下，少量数据的变化会导致系数估计有较大波动。
                图中的每个点代表一次随机抽样后得到的系数估计，点的分散程度反映了系数的不稳定性。
                相关性越强，点的分散程度越大。
                """)
                
            # 建议
            st.markdown("""
            <div class="note-box">
            <h4>商业分析中的实用建议</h4>
            <p>在商业分析中，多重共线性常见于：</p>
            <ul>
                <li>不同广告渠道之间可能高度相关</li>
                <li>产品特征之间可能存在内在关联</li>
                <li>经济指标之间通常有较强的相关性</li>
            </ul>
            <p>实用建议：</p>
            <ul>
                <li>关注重要的商业指标，删除冗余变量</li>
                <li>优先采用简约模型，避免过度拟合</li>
                <li>在报告中明确说明多重共线性的存在及其影响</li>
                <li>关注模型的整体预测能力，而非单个系数值</li>
            </ul>
            </div>
            """, unsafe_allow_html=True)
    
    # 变量选择方法
    with st.expander("变量选择方法", expanded=False):
        st.markdown("""
        <div class="important-box">
        <h4>变量选择的重要性</h4>
        <p>在多元回归分析中，选择合适的自变量集合对于构建良好的模型至关重要：</p>
        <ul>
            <li><b>提高模型解释力</b>：去除无关变量，保留有意义的关系</li>
            <li><b>增强预测准确性</b>：减少过拟合风险，提高模型泛化能力</li>
            <li><b>降低模型复杂度</b>：简化模型，使其更易于理解和应用</li>
            <li><b>节约数据收集成本</b>：识别重要变量，降低数据收集和处理成本</li>
        </ul>
        </div>
        """, unsafe_allow_html=True)
        
        st.markdown("""
        <div class="note-box">
        <h4>常用变量选择方法</h4>
        
        <h5>1. 基于统计检验的方法</h5>
        <ul>
            <li><b>逐步回归</b>：
                <ul>
                    <li>前向逐步法(Forward Selection)：从空模型开始，逐个添加最显著的变量</li>
                    <li>后向消除法(Backward Elimination)：从全模型开始，逐个删除最不显著的变量</li>
                    <li>逐步法(Stepwise)：结合前向和后向，交替添加和删除变量</li>
                </ul>
            </li>
            <li><b>全子集回归</b>：评估所有可能的变量组合，选择最优模型</li>
        </ul>
        
        <h5>2. 基于信息准则的方法</h5>
        <ul>
            <li><b>AIC</b>：Akaike信息准则，平衡拟合优度和模型简洁性</li>
            <li><b>BIC</b>：贝叶斯信息准则，类似AIC但对复杂模型惩罚更严格</li>
            <li><b>调整R²</b>：考虑变量数量的R²修正版，防止过拟合</li>
        </ul>
        
        <h5>3. 正则化方法</h5>
        <ul>
            <li><b>Lasso回归</b>：L1正则化，可将不重要变量的系数压缩为0，自动实现变量选择</li>
            <li><b>岭回归</b>：L2正则化，缩小系数但不会精确为0，主要用于处理多重共线性</li>
            <li><b>弹性网络</b>：结合Lasso和岭回归的优点</li>
        </ul>
        </div>
        """, unsafe_allow_html=True)
        
        st.markdown("""
        <div class="warning-box">
        <h4>变量选择的评价指标</h4>
        <p>选择最优模型时常用的评价指标：</p>
        <ul>
            <li><b>调整R²</b>：调整后的决定系数，考虑变量数量的影响</li>
            <li><b>AIC/BIC</b>：信息准则，数值越小表示模型越优</li>
            <li><b>Mallows' Cp</b>：衡量模型预测准确性和复杂度的平衡</li>
            <li><b>RMSE/预测误差</b>：使用交叉验证评估模型的预测能力</li>
            <li><b>专业领域知识</b>：根据业务理解和专业知识选择变量</li>
        </ul>
        </div>
        """, unsafe_allow_html=True)
        
        # 添加变量选择演示
        st.markdown("#### 变量选择演示")
        
        # 生成多变量数据
        np.random.seed(42)
        n_samples = 100
        
        # 生成5个自变量
        X1 = np.random.uniform(0, 10, n_samples)  # 重要变量
        X2 = np.random.uniform(0, 10, n_samples)  # 重要变量
        X3 = np.random.uniform(0, 10, n_samples)  # 不重要变量
        X4 = X1 * 0.8 + np.random.normal(0, 1, n_samples)  # 与X1高度相关
        X5 = np.random.uniform(0, 10, n_samples)  # 中等重要性变量
        
        # 生成因变量 (只与X1, X2和X5真正相关)
        y = 3 + 2 * X1 + 1.5 * X2 + 0 * X3 + 0 * X4 + 0.5 * X5 + np.random.normal(0, 2, n_samples)
        
        # 创建数据框
        var_sel_data = pd.DataFrame({
            'X1': X1, 'X2': X2, 'X3': X3, 'X4': X4, 'X5': X5, 'y': y
        })
        
        # 显示数据和相关性
        col1, col2 = st.columns([1, 1])
        
        with col1:
            st.write("模拟数据示例:")
            st.dataframe(var_sel_data.head(3))
        
        with col2:
            st.write("变量间相关性:")
            corr_with_y = var_sel_data.corr()['y'].drop('y').sort_values(ascending=False)
            st.dataframe(corr_with_y)
        
        # 可视化不同方法的变量选择结果
        st.markdown("#### 不同方法的变量选择结果")
        
        # 定义不同模型
        models = {
            "全变量模型": ["X1", "X2", "X3", "X4", "X5"],
            "真实相关模型": ["X1", "X2", "X5"],
            "前向逐步法": ["X1", "X2", "X5"],  # 假设结果
            "后向消除法": ["X1", "X2", "X5", "X3"],  # 假设结果
            "Lasso正则化": ["X1", "X2", "X5"]  # 假设结果
        }
        
        # 计算模型评估指标
        model_metrics = {}
        
        for model_name, features in models.items():
            X_model = var_sel_data[features].values
            model = LinearRegression()
            model.fit(X_model, y)
            
            y_pred = model.predict(X_model)
            r2 = model.score(X_model, y)
            adj_r2 = 1 - (1 - r2) * (n_samples - 1) / (n_samples - len(features) - 1)
            rmse = np.sqrt(np.mean((y - y_pred) ** 2))
            
            # 计算简化版AIC (不是真正的AIC，只用于演示)
            aic = n_samples * np.log(np.sum((y - y_pred) ** 2) / n_samples) + 2 * (len(features) + 1)
            
            model_metrics[model_name] = {
                "变量数": len(features),
                "R²": r2,
                "调整R²": adj_r2,
                "RMSE": rmse,
                "简化AIC": aic
            }
        
        # 转换为DataFrame
        metrics_df = pd.DataFrame(model_metrics).T
        metrics_df = metrics_df.round(4)
        
        # 显示评价指标
        st.dataframe(metrics_df)
        
        # 可视化对比
        metric_to_show = st.selectbox("选择指标进行可视化", ["调整R²", "RMSE", "简化AIC"])
        
        # 准备可视化数据
        vis_data = pd.DataFrame({
            "模型": metrics_df.index,
            "指标值": metrics_df[metric_to_show]
        })
        
        # 条形图
        if metric_to_show in ["调整R²"]:
            # 对于R²类指标，值越大越好
            fig = px.bar(vis_data, x="模型", y="指标值", title=f"不同模型的{metric_to_show}对比",
                       color="指标值", color_continuous_scale="Viridis")
        else:
            # 对于RMSE和AIC，值越小越好
            fig = px.bar(vis_data, x="模型", y="指标值", title=f"不同模型的{metric_to_show}对比",
                       color="指标值", color_continuous_scale="Viridis_r")
        
        fig.update_traces(texttemplate='%{y:.4f}', textposition='outside')
        
        st.plotly_chart(fig, use_container_width=True)
        
        # 变量选择建议
        st.markdown("""
        <div class="note-box">
        <h4>商业分析中的变量选择建议</h4>
        <ul>
            <li><b>平衡统计和业务意义</b>：不仅考虑统计显著性，还要考虑业务相关性</li>
            <li><b>避免"数据挖掘"</b>：不要过度依赖自动方法，避免发现无意义的模式</li>
            <li><b>保持模型简约</b>：优先选择简单且合理的模型，而非复杂且微小改进的模型</li>
            <li><b>注意过拟合</b>：使用交叉验证评估泛化能力，而非仅关注训练集性能</li>
            <li><b>关注增量价值</b>：评估每个额外变量带来的预测或解释价值提升</li>
        </ul>
        </div>
        """, unsafe_allow_html=True)
    
    # 交互式多元回归分析
    st.subheader("交互式多元回归分析")
    
    # 数据生成/上传
    data_option = st.selectbox("选择数据", ["广告投入与销售额", "自定义数据"])
    
    if data_option == "广告投入与销售额":
        data = generate_advertising_data(n=100)
        
        # 数据预览
        st.markdown("#### 数据预览")
        st.dataframe(data.head())
        
        # 查看相关性
        st.markdown("#### 变量相关性")
        
        # 计算相关系数
        corr = data.corr().round(3)
        
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
        
        # 散点图矩阵
        st.markdown("#### 散点图矩阵")
        fig_scatter = plot_multiple_regression_scatter_matrix(data, "Sales")
        st.plotly_chart(fig_scatter, use_container_width=True)
        
        # 选择自变量
        st.markdown("#### 选择自变量")
        feature_options = ["TV", "Radio", "Newspaper"]
        selected_features = st.multiselect(
            "选择要包含在模型中的自变量",
            feature_options,
            default=feature_options
        )
        
        if not selected_features:
            st.warning("请至少选择一个自变量")
            st.stop()
        
        # 构建模型
        X = data[selected_features].values
        y = data["Sales"].values
        
        model = LinearRegression()
        model.fit(X, y)
        
        # 模型结果
        st.markdown("#### 回归结果")
        
        # 创建系数表
        coef_df = pd.DataFrame({
            "变量": ["截距"] + selected_features,
            "系数": [model.intercept_] + list(model.coef_)
        })
        
        st.dataframe(coef_df)
        
        # 计算模型评估指标
        y_pred = model.predict(X)
        r2 = model.score(X, y)
        adj_r2 = 1 - (1 - r2) * (len(y) - 1) / (len(y) - len(selected_features) - 1)
        mse = np.mean((y - y_pred) ** 2)
        rmse = np.sqrt(mse)
        
        # 显示评估指标
        col1, col2, col3 = st.columns(3)
        with col1:
            st.metric("R²", f"{r2:.4f}")
        with col2:
            st.metric("调整R²", f"{adj_r2:.4f}")
        with col3:
            st.metric("RMSE", f"{rmse:.4f}")
        
        # 回归方程
        coef_str = " + ".join([f"{model.coef_[i]:.4f} × {feat}" for i, feat in enumerate(selected_features)])
        st.markdown(f"**回归方程**: $y = {model.intercept_:.4f} + {coef_str}$")
        
        # 变量重要性
        st.markdown("#### 变量重要性")
        fig_imp = plot_variable_importance(model, selected_features)
        st.plotly_chart(fig_imp, use_container_width=True)
        
        # 残差分析
        st.markdown("#### 残差分析")
        fig_resid = plot_residuals(model, X, y)
        st.plotly_chart(fig_resid, use_container_width=True)
        
        # 检测多重共线性
        if len(selected_features) > 1:
            st.markdown("#### 多重共线性检测")
            vif_data = calculate_vif(data[selected_features])
            
            vif_df = pd.DataFrame({
                "变量": list(vif_data.keys()),
                "VIF": list(vif_data.values())
            })
            
            # 添加解释信息
            for i, row in vif_df.iterrows():
                if row["VIF"] < 5:
                    vif_df.loc[i, "解释"] = "无明显多重共线性"
                elif row["VIF"] < 10:
                    vif_df.loc[i, "解释"] = "存在中等程度多重共线性"
                else:
                    vif_df.loc[i, "解释"] = "存在严重多重共线性"
            
            st.dataframe(vif_df)
            
            # 提示信息
            if (vif_df["VIF"] > 10).any():
                st.warning("⚠️ 检测到严重的多重共线性问题，可能影响模型解释")
            elif (vif_df["VIF"] > 5).any():
                st.info("ℹ️ 检测到中等程度的多重共线性，建议关注相关变量")
        
        # 3D可视化(如果选择了恰好两个变量)
        if len(selected_features) == 2:
            st.markdown("#### 3D回归平面可视化")
            fig_3d = plot_3d_regression(data, selected_features[0], selected_features[1], "Sales")
            st.plotly_chart(fig_3d, use_container_width=True)
        
        # 预测工具
        st.markdown("#### 预测工具")
        st.write("输入自变量值，预测销售额")
        
        # 创建用于输入预测值的界面
        pred_values = {}
        
        # 根据选择的特征创建输入框
        col_width = min(3, len(selected_features))
        pred_cols = st.columns(col_width)
        
        for i, feature in enumerate(selected_features):
            with pred_cols[i % col_width]:
                min_val = float(data[feature].min())
                max_val = float(data[feature].max())
                step = (max_val - min_val) / 100
                pred_values[feature] = st.number_input(
                    f"{feature}",
                    min_value=min_val,
                    max_value=max_val,
                    value=(min_val + max_val) / 2,
                    step=step
                )
        
        # 预测按钮
        if st.button("预测"):
            # 准备预测数据
            X_pred = np.array([[pred_values[feature] for feature in selected_features]])
            
            # 预测
            prediction = model.predict(X_pred)[0]
            
            # 构建预测区间
            conf_interval = 1.96 * rmse  # 简单的95%置信区间
            
            st.success(f"预测的销售额为: {prediction:.2f}")
            st.info(f"95%预测区间: [{prediction - conf_interval:.2f}, {prediction + conf_interval:.2f}]")
    else:
        # 自定义数据上传
        uploaded_file = st.file_uploader("上传CSV数据文件", type=["csv"])
        
        if uploaded_file is not None:
            # 读取数据
            data = pd.read_csv(uploaded_file)
            
            # 数据预览
            st.markdown("#### 数据预览")
            st.dataframe(data.head())
            
            # 选择因变量
            target_var = st.selectbox("选择因变量", data.columns)
            
            # 选择自变量
            feature_options = [col for col in data.columns if col != target_var]
            selected_features = st.multiselect(
                "选择要包含在模型中的自变量",
                feature_options,
                default=feature_options
            )
            
            if not selected_features:
                st.warning("请至少选择一个自变量")
                st.stop()
            
            # 构建模型
            X = data[selected_features].values
            y = data[target_var].values
            
            # 执行分析
            # ... 与上述代码类似的分析过程
        else:
            st.info("请上传数据文件或选择示例数据")
            st.stop()

if __name__ == "__main__":
    app() 