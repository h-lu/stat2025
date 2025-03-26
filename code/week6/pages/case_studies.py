import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from utils.data_utils import generate_advertising_data, generate_boston_housing_data, generate_customer_segment_data
from utils.plot_utils import plot_simple_regression, plot_multiple_regression_scatter_matrix, plot_residuals, plot_3d_regression, plot_variable_importance

def app():
    """回归分析案例研究页面"""
    
    st.header("回归分析的实际应用案例")
    
    # 选择案例
    case_options = [
        "案例一：房价预测模型",
        "案例二：广告投入效果分析", 
        "案例三：客户细分市场分析"
    ]
    
    selected_case = st.selectbox("选择案例研究", case_options)
    
    if selected_case == "案例一：房价预测模型":
        housing_price_case()
    elif selected_case == "案例二：广告投入效果分析":
        advertising_case()
    else:
        customer_segment_case()

def housing_price_case():
    """房价预测案例"""
    
    st.subheader("案例一：房价预测模型")
    
    st.markdown("""
    ### 业务背景
    
    房地产公司需要建立一个模型来预测房屋价格，以便为定价决策和投资评估提供参考。通过分析影响房价的关键因素，开发一个准确的预测模型。
    """)
    
    # 生成波士顿房价类似数据
    data = generate_boston_housing_data(n=150)
    
    # 数据概览
    with st.expander("数据概览", expanded=True):
        st.dataframe(data.head())
        
        st.markdown("""
        **数据说明**：
        - `rm`: 每套住宅的平均房间数
        - `lstat`: 人口中地位较低人群的百分比
        - `crim`: 城镇人均犯罪率
        - `zn`: 占地面积超过2.5万平方英尺的住宅用地比例
        - `indus`: 每个城镇非零售业务的比例
        - `chas`: 查尔斯河虚拟变量（1表示靠河，0表示不靠河）
        - `nox`: 一氧化氮浓度
        - `age`: 1940年前建成的自住房屋比例
        - `dis`: 距离五个波士顿就业中心的加权距离
        - `rad`: 距离高速公路的可达性指数
        - `tax`: 每一万美元的房产税率
        - `ptratio`: 城镇师生比例
        - `black`: 城镇黑人比例指标
        - `medv`: 房价中位数（万元）
        """)
    
    # 探索性数据分析
    with st.expander("探索性数据分析", expanded=True):
        # 相关性热图
        corr = data.corr().round(2)
        
        fig_corr = px.imshow(
            corr, 
            text_auto=True, 
            color_continuous_scale='RdBu_r',
            title="变量相关性热图"
        )
        
        st.plotly_chart(fig_corr, use_container_width=True)
        
        # 筛选相关性高的变量
        st.markdown("### 主要影响因素与房价的关系")
        
        # 选择变量
        col1, col2 = st.columns(2)
        
        with col1:
            x_var = st.selectbox("选择自变量", ['rm', 'lstat', 'crim', 'zn', 'nox', 'age', 'dis', 'ptratio'])
        
        with col2:
            y_var = 'medv'  # 固定因变量为房价中位数
            st.markdown(f"**因变量**: {y_var} (房价中位数)")
        
        # 可视化
        fig_scatter = px.scatter(
            data, 
            x=x_var, 
            y=y_var, 
            title=f"{x_var}与房价的关系",
            trendline="ols"
        )
        
        st.plotly_chart(fig_scatter, use_container_width=True)
    
    # 建立模型
    with st.expander("模型构建与评估", expanded=True):
        st.markdown("### 多元回归模型")
        
        # 特征选择
        feature_options = ['rm', 'lstat', 'crim', 'zn', 'indus', 'chas', 'nox', 'age', 'dis', 'rad', 'tax', 'ptratio', 'black']
        selected_features = st.multiselect(
            "选择模型特征",
            feature_options,
            default=['rm', 'lstat', 'crim', 'dis', 'ptratio']
        )
        
        if not selected_features:
            st.warning("请至少选择一个特征变量")
            st.stop()
        
        # 准备数据
        X = data[selected_features]
        y = data['medv']
        
        # 训练测试集划分
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)
        
        # 训练模型
        model = LinearRegression()
        model.fit(X_train, y_train)
        
        # 模型评估
        y_pred_train = model.predict(X_train)
        y_pred_test = model.predict(X_test)
        
        train_r2 = r2_score(y_train, y_pred_train)
        test_r2 = r2_score(y_test, y_pred_test)
        
        train_rmse = np.sqrt(mean_squared_error(y_train, y_pred_train))
        test_rmse = np.sqrt(mean_squared_error(y_test, y_pred_test))
        
        # 显示评估指标
        col1, col2 = st.columns(2)
        with col1:
            st.metric("训练集R²", f"{train_r2:.4f}")
            st.metric("测试集R²", f"{test_r2:.4f}")
        
        with col2:
            st.metric("训练集RMSE", f"{train_rmse:.4f}")
            st.metric("测试集RMSE", f"{test_rmse:.4f}")
        
        # 系数分析
        st.markdown("### 变量重要性分析")
        
        coefficients = pd.DataFrame({
            '特征': selected_features,
            '系数': model.coef_
        })
        
        coefficients['绝对值'] = np.abs(coefficients['系数'])
        coefficients = coefficients.sort_values('绝对值', ascending=False)
        
        fig_coef = px.bar(
            coefficients,
            x='特征',
            y='系数',
            title="回归系数大小（重要性）",
            color='系数',
            color_continuous_scale='RdBu',
            text='系数'
        )
        
        fig_coef.update_traces(texttemplate='%{text:.3f}', textposition='outside')
        st.plotly_chart(fig_coef, use_container_width=True)
        
        # 3D可视化 (如果选择了至少两个特征)
        if len(selected_features) >= 2:
            st.markdown("### 3D可视化")
            
            features_3d = st.selectbox(
                "选择两个特征进行3D可视化",
                [(f1, f2) for i, f1 in enumerate(selected_features) for f2 in selected_features[i+1:]], 
                format_func=lambda x: f"{x[0]} 和 {x[1]}"
            )
            
            fig_3d = plot_3d_regression(data, features_3d[0], features_3d[1], 'medv', 
                                      title=f"{features_3d[0]}、{features_3d[1]}与房价的关系")
            
            st.plotly_chart(fig_3d, use_container_width=True)
    
    # 业务应用
    with st.expander("业务应用", expanded=True):
        st.markdown("### 房价预测工具")
        
        st.markdown("""
        使用此工具估算特定条件下的房价。调整各项特征参数，查看预测结果。
        """)
        
        # 创建输入字段
        input_data = {}
        
        # 每行放置2个输入控件
        for i in range(0, len(selected_features), 2):
            col1, col2 = st.columns(2)
            
            with col1:
                feature = selected_features[i]
                min_val = float(data[feature].min())
                max_val = float(data[feature].max())
                mean_val = float(data[feature].mean())
                
                if feature == 'chas':  # 二元变量特殊处理
                    input_data[feature] = st.selectbox(f"{feature}", [0, 1], index=0)
                else:
                    input_data[feature] = st.slider(
                        f"{feature}", 
                        min_val,
                        max_val,
                        mean_val
                    )
            
            with col2:
                if i + 1 < len(selected_features):
                    feature = selected_features[i + 1]
                    min_val = float(data[feature].min())
                    max_val = float(data[feature].max())
                    mean_val = float(data[feature].mean())
                    
                    if feature == 'chas':  # 二元变量特殊处理
                        input_data[feature] = st.selectbox(f"{feature}", [0, 1], index=0)
                    else:
                        input_data[feature] = st.slider(
                            f"{feature}", 
                            min_val,
                            max_val,
                            mean_val
                        )
        
        # 预测
        if st.button("预测房价"):
            input_df = pd.DataFrame([input_data])
            prediction = model.predict(input_df)[0]
            
            st.success(f"预测房价: {prediction:.2f}万元")
            
            # 提供一些业务解读
            st.markdown("### 业务解读")
            st.markdown(f"""
            根据所选特征和输入数据，模型预测该房产的价格为**{prediction:.2f}万元**。
            
            影响房价的主要因素为：
            - {'、'.join(coefficients['特征'].head(3).tolist())}
            
            如果您想提高房产价值，可以考虑改善这些方面。此预测可用于：
            - 房产定价参考
            - 投资决策支持
            - 房产评估标准制定
            """)

def advertising_case():
    """广告投入效果分析案例"""
    
    st.subheader("案例二：广告投入效果分析")
    
    st.markdown("""
    ### 业务背景
    
    一家零售公司想要了解不同广告渠道的投入如何影响销售业绩，以优化其广告预算分配。
    通过分析电视、广播和报纸广告的支出数据，确定哪些渠道最有效，并根据结果制定预算策略。
    """)
    
    # 生成广告数据
    data = generate_advertising_data(n=200)
    
    # 数据概览
    with st.expander("数据概览", expanded=True):
        st.dataframe(data.head())
        
        st.markdown("""
        **数据说明**：
        - `TV`: 电视广告支出（万元）
        - `Radio`: 广播广告支出（万元）
        - `Newspaper`: 报纸广告支出（万元）
        - `Sales`: 销售额（万元）
        """)
    
    # 探索性数据分析
    with st.expander("探索性数据分析", expanded=True):
        # 相关性矩阵
        corr = data.corr().round(3)
        
        fig_corr = px.imshow(
            corr, 
            text_auto=True, 
            color_continuous_scale='viridis',
            title="广告投入与销售额相关性矩阵"
        )
        
        st.plotly_chart(fig_corr, use_container_width=True)
        
        # 单因素分析
        st.markdown("### 各广告渠道与销售额的关系")
        
        col1, col2 = st.columns(2)
        
        with col1:
            channel = st.selectbox("选择广告渠道", ["TV", "Radio", "Newspaper"])
        
        # 散点图 + 趋势线
        fig_scatter = px.scatter(
            data, 
            x=channel, 
            y="Sales", 
            title=f"{channel}广告投入与销售额关系",
            labels={channel: f"{channel}广告支出（万元）", "Sales": "销售额（万元）"},
            trendline="ols"
        )
        
        st.plotly_chart(fig_scatter, use_container_width=True)
        
        # 多变量分析
        st.markdown("### 多渠道广告投入组合分析")
        
        fig_3d = plot_3d_regression(
            data, 
            "TV", 
            "Radio", 
            "Sales", 
            title="电视广告、广播广告与销售额关系"
        )
        
        st.plotly_chart(fig_3d, use_container_width=True)
    
    # 模型构建与评估
    with st.expander("模型构建与评估", expanded=True):
        st.markdown("### 单渠道模型对比")
        
        # 对比不同单因素模型
        models_data = {}
        metrics = ['截距', '系数', 'R²', 'RMSE']
        
        for channel in ["TV", "Radio", "Newspaper"]:
            X = data[channel].values.reshape(-1, 1)
            y = data["Sales"].values
            
            model = LinearRegression()
            model.fit(X, y)
            
            predictions = model.predict(X)
            r2 = r2_score(y, predictions)
            rmse = np.sqrt(mean_squared_error(y, predictions))
            
            models_data[channel] = [model.intercept_, model.coef_[0], r2, rmse]
        
        # 创建比较表格
        comparison_df = pd.DataFrame.from_dict(models_data, orient='index', columns=metrics)
        comparison_df = comparison_df.round(4)
        
        st.dataframe(comparison_df)
        
        # 折线图比较
        fig_comparison = px.bar(
            comparison_df.reset_index(),
            x='index',
            y='R²',
            title="各广告渠道R²对比",
            labels={'index': '广告渠道', 'R²': 'R²值'},
            color='R²',
            text='R²'
        )
        
        fig_comparison.update_traces(texttemplate='%{text:.3f}', textposition='outside')
        st.plotly_chart(fig_comparison, use_container_width=True)
        
        # 多元回归模型
        st.markdown("### 多元回归模型")
        
        channels = st.multiselect(
            "选择广告渠道组合",
            ["TV", "Radio", "Newspaper"],
            default=["TV", "Radio", "Newspaper"]
        )
        
        if channels:
            X_multi = data[channels]
            y_multi = data["Sales"]
            
            X_train, X_test, y_train, y_test = train_test_split(X_multi, y_multi, test_size=0.3, random_state=42)
            
            model_multi = LinearRegression()
            model_multi.fit(X_train, y_train)
            
            # 评估
            y_pred_train = model_multi.predict(X_train)
            y_pred_test = model_multi.predict(X_test)
            
            train_r2 = r2_score(y_train, y_pred_train)
            test_r2 = r2_score(y_test, y_pred_test)
            
            train_rmse = np.sqrt(mean_squared_error(y_train, y_pred_train))
            test_rmse = np.sqrt(mean_squared_error(y_test, y_pred_test))
            
            # 显示评估指标
            col1, col2 = st.columns(2)
            with col1:
                st.metric("训练集R²", f"{train_r2:.4f}")
                st.metric("测试集R²", f"{test_r2:.4f}")
            
            with col2:
                st.metric("训练集RMSE", f"{train_rmse:.4f}")
                st.metric("测试集RMSE", f"{test_rmse:.4f}")
            
            # 变量重要性
            coefs = model_multi.coef_
            feature_importance = pd.DataFrame({
                '特征': channels,
                '系数': coefs,
                '标准化系数': coefs * np.std(X_multi, axis=0) / np.std(y_multi)
            })
            
            feature_importance = feature_importance.sort_values('标准化系数', ascending=False)
            
            fig_importance = px.bar(
                feature_importance,
                x='特征',
                y='标准化系数',
                title="广告渠道重要性（标准化系数）",
                color='标准化系数',
                text='标准化系数'
            )
            
            fig_importance.update_traces(texttemplate='%{text:.3f}', textposition='outside')
            st.plotly_chart(fig_importance, use_container_width=True)
            
            # 残差分析
            st.markdown("### 残差分析")
            
            residuals = y_test - y_pred_test
            
            fig_residuals = px.scatter(
                x=y_pred_test,
                y=residuals,
                labels={'x': '预测值', 'y': '残差'},
                title="残差散点图"
            )
            
            fig_residuals.add_hline(y=0, line_dash="dash", line_color="red")
            st.plotly_chart(fig_residuals, use_container_width=True)
    
    # 业务应用
    with st.expander("业务应用", expanded=True):
        st.markdown("### 广告预算优化工具")
        
        st.markdown("""
        使用此工具来优化广告预算分配。根据不同投入水平预测销售额，找到最佳投入组合。
        """)
        
        # 创建预算分配工具
        total_budget = st.slider("总广告预算（万元）", 10.0, 100.0, 50.0, 5.0)
        
        col1, col2, col3 = st.columns(3)
        
        with col1:
            tv_percent = st.slider("电视广告占比", 0.0, 1.0, 0.6, 0.05)
            tv_budget = total_budget * tv_percent
            st.metric("电视广告预算", f"{tv_budget:.1f}万元")
        
        with col2:
            radio_percent = st.slider("广播广告占比", 0.0, 1.0 - tv_percent, 0.3, 0.05)
            radio_budget = total_budget * radio_percent
            st.metric("广播广告预算", f"{radio_budget:.1f}万元")
        
        with col3:
            newspaper_percent = 1.0 - tv_percent - radio_percent
            newspaper_budget = total_budget * newspaper_percent
            st.metric("报纸广告预算", f"{newspaper_budget:.1f}万元")
        
        # 预测销售额
        X_multi = data[["TV", "Radio", "Newspaper"]]
        y_multi = data["Sales"]
        
        model_multi = LinearRegression()
        model_multi.fit(X_multi, y_multi)
        
        input_data = np.array([[tv_budget, radio_budget, newspaper_budget]])
        predicted_sales = model_multi.predict(input_data)[0]
        
        st.success(f"预计销售额: {predicted_sales:.2f}万元")
        
        # 可视化预算分配
        budget_df = pd.DataFrame({
            '渠道': ['电视', '广播', '报纸'],
            '预算': [tv_budget, radio_budget, newspaper_budget]
        })
        
        fig_budget = px.pie(
            budget_df,
            values='预算',
            names='渠道',
            title="广告预算分配",
            hole=0.3
        )
        
        st.plotly_chart(fig_budget, use_container_width=True)
        
        # 边际收益分析
        st.markdown("### 边际收益分析")
        
        st.markdown("""
        各广告渠道的边际收益（每增加1万元投入带来的销售额增长）：
        - 电视广告: {:.2f}万元
        - 广播广告: {:.2f}万元
        - 报纸广告: {:.2f}万元
        
        **建议**：基于边际收益，优先增加{:s}广告的投入可能带来更高的回报。
        """.format(
            model_multi.coef_[0], 
            model_multi.coef_[1], 
            model_multi.coef_[2],
            ["电视", "广播", "报纸"][np.argmax(model_multi.coef_)]
        ))

def customer_segment_case():
    """客户细分市场分析案例"""
    
    st.subheader("案例三：客户细分市场分析")
    
    st.markdown("""
    ### 业务背景
    
    一家零售企业想要了解不同客户细分市场对产品价格、促销和店铺质量的敏感度。
    通过分析不同客户群体的购买行为，开发针对性的营销策略，提高各细分市场的销售额。
    """)
    
    # 生成客户细分数据
    data = generate_customer_segment_data(n=30)
    
    # 数据概览
    with st.expander("数据概览", expanded=True):
        st.dataframe(data.head())
        
        st.markdown("""
        **数据说明**：
        - `customer_segment`: 客户细分市场（高收入、中收入、低收入）
        - `price`: 产品价格（元）
        - `promotion`: 促销力度（0-1，1表示最大力度）
        - `store_quality`: 店铺质量评分（1-5分）
        - `purchase`: 购买量
        """)
    
    # 探索性数据分析
    with st.expander("探索性数据分析", expanded=True):
        # 比较不同客户群体的购买量
        fig_box = px.box(
            data,
            x="customer_segment",
            y="purchase",
            color="customer_segment",
            title="不同客户群体的购买量分布",
            labels={"customer_segment": "客户群体", "purchase": "购买量"}
        )
        
        st.plotly_chart(fig_box, use_container_width=True)
        
        # 相关性分析（按客户细分）
        segment = st.selectbox("选择客户细分市场", ["高收入", "中收入", "低收入", "全部"])
        
        if segment != "全部":
            segment_data = data[data["customer_segment"] == segment]
        else:
            segment_data = data
        
        # 散点图矩阵
        fig_scatter_matrix = px.scatter_matrix(
            segment_data,
            dimensions=["price", "promotion", "store_quality", "purchase"],
            color="customer_segment",
            title=f"{segment if segment != '全部' else '全部客户'}的变量关系矩阵",
            labels={
                "price": "价格", 
                "promotion": "促销力度", 
                "store_quality": "店铺质量", 
                "purchase": "购买量"
            }
        )
        
        st.plotly_chart(fig_scatter_matrix, use_container_width=True)
    
    # 模型构建与评估
    with st.expander("模型构建与评估", expanded=True):
        st.markdown("### 客户细分市场回归模型")
        
        # 为每个细分市场构建单独的回归模型
        segments = ["高收入", "中收入", "低收入"]
        models = {}
        
        for segment in segments:
            segment_data = data[data["customer_segment"] == segment]
            
            X = segment_data[["price", "promotion", "store_quality"]]
            y = segment_data["purchase"]
            
            model = LinearRegression()
            model.fit(X, y)
            
            y_pred = model.predict(X)
            r2 = r2_score(y, y_pred)
            rmse = np.sqrt(mean_squared_error(y, y_pred))
            
            models[segment] = {
                "模型": model,
                "R²": r2,
                "RMSE": rmse,
                "系数": {
                    "截距": model.intercept_,
                    "价格": model.coef_[0],
                    "促销": model.coef_[1],
                    "店铺质量": model.coef_[2]
                }
            }
        
        # 显示模型性能比较
        performance_df = pd.DataFrame({
            "客户群体": segments,
            "R²": [models[segment]["R²"] for segment in segments],
            "RMSE": [models[segment]["RMSE"] for segment in segments]
        })
        
        st.dataframe(performance_df.round(3))
        
        # 系数比较
        coef_df = pd.DataFrame({
            "因素": ["价格", "促销", "店铺质量"],
            "高收入": [models["高收入"]["系数"]["价格"], 
                     models["高收入"]["系数"]["促销"], 
                     models["高收入"]["系数"]["店铺质量"]],
            "中收入": [models["中收入"]["系数"]["价格"], 
                     models["中收入"]["系数"]["促销"], 
                     models["中收入"]["系数"]["店铺质量"]],
            "低收入": [models["低收入"]["系数"]["价格"], 
                     models["低收入"]["系数"]["促销"], 
                     models["低收入"]["系数"]["店铺质量"]]
        })
        
        st.dataframe(coef_df.round(3))
        
        # 可视化系数比较
        st.markdown("### 不同客户群体对各因素的敏感度比较")
        
        # 转换数据格式用于绘图
        coef_long = pd.melt(
            coef_df, 
            id_vars=["因素"], 
            value_vars=["高收入", "中收入", "低收入"],
            var_name="客户群体", 
            value_name="系数"
        )
        
        fig_coef = px.bar(
            coef_long,
            x="因素",
            y="系数",
            color="客户群体",
            barmode="group",
            title="不同客户群体对价格、促销和店铺质量的敏感度",
            labels={"因素": "影响因素", "系数": "敏感度系数"}
        )
        
        st.plotly_chart(fig_coef, use_container_width=True)
    
    # 业务应用
    with st.expander("业务应用", expanded=True):
        st.markdown("### 客户群体敏感度分析工具")
        
        st.markdown("""
        使用此工具分析不同定价、促销策略和店铺质量对各客户群体购买量的影响。
        """)
        
        col1, col2, col3 = st.columns(3)
        
        with col1:
            price = st.slider("产品价格", 50.0, 200.0, 100.0, 5.0)
        
        with col2:
            promotion = st.slider("促销力度", 0.0, 1.0, 0.5, 0.1)
        
        with col3:
            store_quality = st.slider("店铺质量", 1.0, 5.0, 3.0, 0.5)
        
        # 预测各客户群体的购买量
        predictions = {}
        
        for segment in segments:
            model = models[segment]["模型"]
            pred = model.predict([[price, promotion, store_quality]])[0]
            predictions[segment] = pred
        
        # 显示预测结果
        pred_df = pd.DataFrame({
            "客户群体": segments,
            "预计购买量": [predictions[segment] for segment in segments]
        })
        
        st.dataframe(pred_df.round(2))
        
        # 可视化预测结果
        fig_pred = px.bar(
            pred_df,
            x="客户群体",
            y="预计购买量",
            color="客户群体",
            title=f"当前设置下各客户群体的预计购买量",
            text="预计购买量"
        )
        
        fig_pred.update_traces(texttemplate='%{text:.2f}', textposition='outside')
        st.plotly_chart(fig_pred, use_container_width=True)
        
        # 业务建议
        st.markdown("### 业务建议")
        
        # 确定最敏感的因素
        price_sensitivity = {segment: abs(models[segment]["系数"]["价格"]) for segment in segments}
        promo_sensitivity = {segment: abs(models[segment]["系数"]["促销"]) for segment in segments}
        quality_sensitivity = {segment: abs(models[segment]["系数"]["店铺质量"]) for segment in segments}
        
        most_price_sensitive = max(price_sensitivity, key=price_sensitivity.get)
        most_promo_sensitive = max(promo_sensitivity, key=promo_sensitivity.get)
        most_quality_sensitive = max(quality_sensitivity, key=quality_sensitivity.get)
        
        st.markdown(f"""
        根据模型分析，我们建议以下针对性策略：
        
        1. **价格策略**：
           - 最价格敏感的群体是**{most_price_sensitive}**客户
           - 为该群体提供更多价格优惠和折扣
        
        2. **促销策略**：
           - **{most_promo_sensitive}**客户对促销活动反应最强烈
           - 针对该群体增加促销频率和力度
        
        3. **店铺体验**：
           - **{most_quality_sensitive}**客户最看重店铺质量
           - 提升针对该群体购物场所的体验和服务
        
        4. **差异化营销**：
           - 高收入客户：强调产品品质和店铺体验，降低促销依赖
           - 中收入客户：平衡价格和品质，适度促销
           - 低收入客户：提供有竞争力的价格和高性价比产品
        """)

if __name__ == "__main__":
    app() 