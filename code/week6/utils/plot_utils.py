import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import statsmodels.api as sm
from sklearn.linear_model import LinearRegression

def plot_simple_regression(X, y, x_label='X', y_label='Y', title='简单线性回归', add_line=True):
    """
    绘制简单线性回归散点图和回归线
    
    参数:
    X: 自变量数据
    y: 因变量数据
    x_label: X轴标签
    y_label: Y轴标签
    title: 图表标题
    add_line: 是否添加回归线
    
    返回:
    plotly图表对象
    """
    fig = px.scatter(x=X, y=y, labels={'x': x_label, 'y': y_label})
    
    if add_line:
        # 拟合简单线性回归模型
        model = LinearRegression()
        model.fit(X.reshape(-1, 1), y)
        
        # 预测值
        x_range = np.linspace(X.min(), X.max(), 100)
        y_pred = model.predict(x_range.reshape(-1, 1))
        
        # 添加回归线
        fig.add_trace(go.Scatter(
            x=x_range,
            y=y_pred,
            mode='lines',
            name=f'y = {model.intercept_:.2f} + {model.coef_[0]:.2f}x'
        ))
        
        # 更新布局
        fig.update_layout(
            title=title,
            xaxis_title=x_label,
            yaxis_title=y_label,
            legend_title="模型",
            template="plotly_white"
        )
    
    return fig

def plot_multiple_regression_scatter_matrix(df, target=None):
    """
    绘制多元回归的散点图矩阵
    
    参数:
    df: 包含特征和目标变量的DataFrame
    target: 目标变量的列名
    
    返回:
    plotly图表对象
    """
    if target:
        # 高亮目标变量
        dimensions = list(df.columns)
        dimensions.remove(target)
        dimensions = [target] + dimensions
        
        fig = px.scatter_matrix(
            df,
            dimensions=dimensions,
            color=target,
            color_continuous_scale=px.colors.sequential.Viridis,
            opacity=0.8
        )
    else:
        fig = px.scatter_matrix(
            df,
            opacity=0.8
        )
    
    # 更新布局
    fig.update_layout(
        title="变量散点图矩阵",
        template="plotly_white"
    )
    
    # 更新对角线图表为直方图
    fig.update_traces(
        diagonal_visible=True,
        showupperhalf=False
    )
    
    return fig

def plot_residuals(model, X, y, title="残差分析"):
    """
    绘制残差分析图
    
    参数:
    model: 已拟合的回归模型
    X: 特征数据
    y: 目标变量
    title: 图表标题
    
    返回:
    plotly图表对象
    """
    # 预测值
    y_pred = model.predict(X)
    
    # 残差
    residuals = y - y_pred
    
    # 创建子图
    fig = make_subplots(rows=2, cols=2,
                        specs=[[{"type": "scatter"}, {"type": "scatter"}],
                               [{"type": "histogram"}, {"type": "scatter"}]],
                        subplot_titles=("残差 vs 预测值", "残差 vs 拟合值",
                                        "残差分布", "QQ图"))
    
    # 1. 残差 vs 预测值
    fig.add_trace(go.Scatter(
        x=y_pred,
        y=residuals,
        mode='markers',
        marker=dict(color='blue'),
        name='残差'
    ), row=1, col=1)
    
    # 添加水平线（y=0）
    fig.add_shape(
        type="line",
        x0=min(y_pred),
        y0=0,
        x1=max(y_pred),
        y1=0,
        line=dict(color="red", width=2, dash="dash"),
        row=1, col=1
    )
    
    # 2. 残差 vs 拟合值（可视化非线性关系）
    fig.add_trace(go.Scatter(
        x=y_pred,
        y=np.sqrt(np.abs(residuals)),
        mode='markers',
        marker=dict(color='green'),
        name='缩放残差'
    ), row=1, col=2)
    
    # 3. 残差直方图
    fig.add_trace(go.Histogram(
        x=residuals,
        marker=dict(color='blue'),
        name='残差分布'
    ), row=2, col=1)
    
    # 4. QQ图（检验正态性）
    # 计算理论分位数
    sorted_residuals = np.sort(residuals)
    n = len(sorted_residuals)
    theoretical_quantiles = np.array([(i - 0.5) / n for i in range(1, n + 1)])
    theoretical_quantiles = sm.distributions.norm.ppf(theoretical_quantiles)
    
    fig.add_trace(go.Scatter(
        x=theoretical_quantiles,
        y=sorted_residuals,
        mode='markers',
        marker=dict(color='purple'),
        name='QQ图'
    ), row=2, col=2)
    
    # 添加QQ参考线
    slope = np.std(sorted_residuals)
    intercept = np.mean(sorted_residuals)
    fig.add_shape(
        type="line",
        x0=min(theoretical_quantiles),
        y0=intercept + slope * min(theoretical_quantiles),
        x1=max(theoretical_quantiles),
        y1=intercept + slope * max(theoretical_quantiles),
        line=dict(color="red", width=2, dash="dash"),
        row=2, col=2
    )
    
    # 更新布局
    fig.update_layout(
        title=title,
        height=800,
        template="plotly_white"
    )
    
    fig.update_xaxes(title_text="预测值", row=1, col=1)
    fig.update_yaxes(title_text="残差", row=1, col=1)
    
    fig.update_xaxes(title_text="预测值", row=1, col=2)
    fig.update_yaxes(title_text="√|残差|", row=1, col=2)
    
    fig.update_xaxes(title_text="残差值", row=2, col=1)
    fig.update_yaxes(title_text="频数", row=2, col=1)
    
    fig.update_xaxes(title_text="理论分位数", row=2, col=2)
    fig.update_yaxes(title_text="样本分位数", row=2, col=2)
    
    return fig

def plot_variable_importance(model, feature_names, title="变量重要性"):
    """
    绘制变量重要性图表
    
    参数:
    model: 已拟合的回归模型
    feature_names: 特征名称列表
    title: 图表标题
    
    返回:
    plotly图表对象
    """
    # 获取系数绝对值
    importance = np.abs(model.coef_)
    
    # 创建数据框
    df = pd.DataFrame({
        'feature': feature_names,
        'importance': importance
    })
    
    # 按重要性排序
    df = df.sort_values('importance', ascending=False)
    
    # 创建条形图
    fig = px.bar(
        df,
        x='feature',
        y='importance',
        color='importance',
        color_continuous_scale='Viridis',
        text=df['importance'].round(3),
        title=title
    )
    
    # 更新布局
    fig.update_layout(
        xaxis_title="特征",
        yaxis_title="系数绝对值",
        yaxis=dict(tickformat='.3f'),
        template="plotly_white"
    )
    
    return fig

def plot_3d_regression(df, x1, x2, y, title="3D回归模型"):
    """
    绘制3D回归图表
    
    参数:
    df: 数据框
    x1: 第一个自变量
    x2: 第二个自变量
    y: 因变量
    title: 图表标题
    
    返回:
    plotly图表对象
    """
    # 提取数据
    X = df[[x1, x2]].values
    y_values = df[y].values
    
    # 拟合回归模型
    model = LinearRegression()
    model.fit(X, y_values)
    
    # 创建网格点
    grid_lines = 50
    x1_range = np.linspace(df[x1].min(), df[x1].max(), grid_lines)
    x2_range = np.linspace(df[x2].min(), df[x2].max(), grid_lines)
    x1_grid, x2_grid = np.meshgrid(x1_range, x2_range)
    
    # 预测值
    grid_points = np.column_stack((x1_grid.flatten(), x2_grid.flatten()))
    z_pred = model.predict(grid_points).reshape(grid_lines, grid_lines)
    
    # 创建3D图
    fig = go.Figure()
    
    # 添加散点图
    fig.add_trace(go.Scatter3d(
        x=df[x1],
        y=df[x2],
        z=df[y],
        mode='markers',
        marker=dict(size=5, color='blue', opacity=0.7),
        name='观测值'
    ))
    
    # 添加回归平面
    fig.add_trace(go.Surface(
        x=x1_range,
        y=x2_range,
        z=z_pred,
        opacity=0.7,
        colorscale='Viridis',
        name='回归平面'
    ))
    
    # 更新布局
    fig.update_layout(
        title=title,
        scene=dict(
            xaxis_title=x1,
            yaxis_title=x2,
            zaxis_title=y
        ),
        template="plotly_white",
        margin=dict(r=20, l=10, b=10, t=50)
    )
    
    return fig

def plot_variable_selection_results(models_dict, metric='r2'):
    """
    绘制变量选择结果的比较图
    
    参数:
    models_dict: 包含模型名称和性能指标的字典
    metric: 要比较的性能指标
    
    返回:
    plotly图表对象
    """
    models = list(models_dict.keys())
    
    if metric == 'r2':
        values = [models_dict[m]['R²'] for m in models]
        title = "模型R²比较"
        yaxis_title = "R²"
    elif metric == 'rmse':
        values = [models_dict[m]['RMSE'] for m in models]
        title = "模型RMSE比较"
        yaxis_title = "RMSE"
    else:
        values = [models_dict[m][metric] for m in models]
        title = f"模型{metric}比较"
        yaxis_title = metric
    
    fig = px.bar(
        x=models,
        y=values,
        color=values,
        labels={'x': '模型', 'y': yaxis_title},
        title=title
    )
    
    # 更新布局
    fig.update_layout(
        xaxis_title="模型",
        yaxis_title=yaxis_title,
        template="plotly_white"
    )
    
    return fig 