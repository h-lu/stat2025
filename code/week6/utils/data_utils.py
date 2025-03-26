import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score

def generate_advertising_data(n=100, seed=123):
    """
    生成广告投入与销售额数据
    
    参数:
    n: 样本数量
    seed: 随机数种子
    
    返回:
    pandas DataFrame
    """
    np.random.seed(seed)
    tv = np.random.uniform(5, 50, n)
    radio = np.random.uniform(1, 30, n)
    newspaper = np.random.uniform(0, 20, n)
    
    # 生成销售额，添加一些随机噪声
    sales = 5 + 0.15 * tv + 0.3 * radio + 0.1 * newspaper + np.random.normal(0, 2, n)
    
    return pd.DataFrame({
        'TV': tv,
        'Radio': radio,
        'Newspaper': newspaper,
        'Sales': sales
    })

def generate_boston_housing_data(n=100, seed=123):
    """
    生成类似波士顿房价的模拟数据
    
    参数:
    n: 样本数量
    seed: 随机数种子
    
    返回:
    pandas DataFrame
    """
    np.random.seed(seed)
    
    rm = np.random.uniform(4, 9, n)  # 每套住宅的平均房间数
    lstat = np.random.uniform(1, 30, n)  # 人口中地位较低人群的百分比
    crim = np.random.uniform(0.01, 20, n)  # 城镇人均犯罪率
    zn = np.random.uniform(0, 100, n)  # 占地面积超过2.5万平方英尺的住宅用地比例
    indus = np.random.uniform(0, 30, n)  # 每个城镇非零售业务的比例
    chas = np.random.binomial(1, 0.1, n)  # 查尔斯河虚拟变量
    nox = np.random.uniform(0.3, 0.9, n)  # 一氧化氮浓度
    age = np.random.uniform(10, 100, n)  # 1940年前建成的自住房屋比例
    dis = np.random.uniform(1, 10, n)  # 距离五个波士顿就业中心的加权距离
    rad = np.random.randint(1, 25, n)  # 距离高速公路的可达性指数
    tax = np.random.uniform(200, 700, n)  # 每一万美元的房产税率
    ptratio = np.random.uniform(12, 22, n)  # 城镇师生比例
    black = np.random.uniform(0, 400, n)  # 1000(Bk - 0.63)^2，Bk是城镇黑人比例
    
    # 生成房价中位数，添加一些随机噪声
    medv = 10 + 5 * rm - 0.6 * lstat - 0.3 * crim + 0.01 * zn - 0.1 * indus + 2 * chas \
           - 15 * nox - 0.01 * age - 0.5 * dis + 0.1 * rad - 0.01 * tax - 0.5 * ptratio \
           + 0.01 * black + np.random.normal(0, 3, n)
    
    # 确保房价为正
    medv = np.maximum(medv, 5)
    
    return pd.DataFrame({
        'rm': rm,
        'lstat': lstat,
        'crim': crim,
        'zn': zn,
        'indus': indus,
        'chas': chas,
        'nox': nox,
        'age': age,
        'dis': dis,
        'rad': rad,
        'tax': tax,
        'ptratio': ptratio,
        'black': black,
        'medv': medv
    })

def generate_product_marketing_data(n=30, seed=123):
    """
    生成产品营销效果数据
    
    参数:
    n: 每种产品的样本数量
    seed: 随机数种子
    
    返回:
    pandas DataFrame
    """
    np.random.seed(seed)
    
    products = ['A', 'B', 'C']
    data = []
    
    for product in products:
        tv = np.random.uniform(5, 50, n)
        radio = np.random.uniform(1, 30, n)
        newspaper = np.random.uniform(0, 20, n)
        
        # 不同产品的广告效果不同
        if product == 'A':
            tv_coef, radio_coef, newspaper_coef = 0.2, 0.15, 0.05
        elif product == 'B':
            tv_coef, radio_coef, newspaper_coef = 0.1, 0.3, 0.1
        else:  # product == 'C'
            tv_coef, radio_coef, newspaper_coef = 0.05, 0.1, 0.2
        
        sales = 5 + tv_coef * tv + radio_coef * radio + newspaper_coef * newspaper + np.random.normal(0, 2, n)
        
        for i in range(n):
            data.append({
                'product': product,
                'TV': tv[i],
                'Radio': radio[i],
                'Newspaper': newspaper[i],
                'Sales': sales[i]
            })
    
    return pd.DataFrame(data)

def generate_customer_segment_data(n=30, seed=123):
    """
    生成不同客户细分市场的数据
    
    参数:
    n: 每个细分市场的样本数量
    seed: 随机数种子
    
    返回:
    pandas DataFrame
    """
    np.random.seed(seed)
    
    segments = ['高收入', '中收入', '低收入']
    data = []
    
    for segment in segments:
        price = np.random.uniform(50, 200, n)
        promotion = np.random.uniform(0, 1, n)
        store_quality = np.random.uniform(1, 5, n)
        
        # 不同细分市场的价格敏感度和促销敏感度不同
        if segment == '高收入':
            price_coef, promotion_coef = -0.01, 1
        elif segment == '中收入':
            price_coef, promotion_coef = -0.03, 2
        else:  # segment == '低收入'
            price_coef, promotion_coef = -0.05, 3
        
        purchase = 10 + price_coef * price + promotion_coef * promotion + 0.5 * store_quality + np.random.normal(0, 1, n)
        
        for i in range(n):
            data.append({
                'customer_segment': segment,
                'price': price[i],
                'promotion': promotion[i],
                'store_quality': store_quality[i],
                'purchase': purchase[i]
            })
    
    return pd.DataFrame(data)

def calculate_regression_metrics(model, X, y):
    """
    计算回归模型的评估指标
    
    参数:
    model: 已拟合的回归模型
    X: 特征数据
    y: 目标变量
    
    返回:
    字典，包含各种评估指标
    """
    y_pred = model.predict(X)
    
    metrics = {
        'R²': r2_score(y, y_pred),
        'MSE': mean_squared_error(y, y_pred),
        'RMSE': np.sqrt(mean_squared_error(y, y_pred)),
        'MAE': np.mean(np.abs(y - y_pred)),
        '系数': model.coef_,
        '截距': model.intercept_
    }
    
    return metrics

def calculate_vif(X):
    """
    计算特征的方差膨胀因子(VIF)
    
    参数:
    X: 特征数据(DataFrame)
    
    返回:
    字典，包含每个特征的VIF
    """
    # 需要确保X是DataFrame类型
    if not isinstance(X, pd.DataFrame):
        X = pd.DataFrame(X)
    
    vif_data = {}
    
    for i in range(X.shape[1]):
        # 选取所有其他特征作为自变量
        X_other = X.drop(X.columns[i], axis=1)
        # 选取当前特征作为因变量
        y = X.iloc[:, i]
        
        # 拟合回归模型
        model = LinearRegression()
        model.fit(X_other, y)
        
        # 计算R²
        r_squared = model.score(X_other, y)
        
        # 计算VIF
        vif = 1 / (1 - r_squared) if r_squared < 1 else float('inf')
        
        vif_data[X.columns[i]] = vif
    
    return vif_data 