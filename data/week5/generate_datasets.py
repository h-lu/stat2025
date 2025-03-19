#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import os

# 设置随机种子，确保结果可复现
np.random.seed(42)

# 确保输出目录存在
output_dir = '.'
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# =====================================================
# 练习1：市场营销策略评估
# =====================================================
"""
某电商平台测试了四种不同的营销策略（A：折扣优惠，B：会员积分，C：免费赠品，D：限时秒杀），
每种策略在10个不同城市实施。收集了每种策略的点击率数据。
"""

# 设置均值和标准差使数据有明显差异
strategy_means = {
    'A': 0.08,  # 折扣优惠
    'B': 0.06,  # 会员积分
    'C': 0.05,  # 免费赠品
    'D': 0.09   # 限时秒杀
}
strategy_stds = {
    'A': 0.015,
    'B': 0.012,
    'C': 0.010,
    'D': 0.020
}

# 生成城市名称
cities = [f'城市{i+1}' for i in range(10)]

# 创建数据框
data_exercise1 = []

for strategy, mean in strategy_means.items():
    std = strategy_stds[strategy]
    # 生成点击率数据 (10个城市)
    clickthrough_rates = np.random.normal(mean, std, 10)
    # 确保点击率在合理范围内 (0到1之间)
    clickthrough_rates = np.clip(clickthrough_rates, 0.01, 0.2)
    
    for i, city in enumerate(cities):
        data_exercise1.append({
            'strategy': strategy,
            'strategy_name': {
                'A': '折扣优惠',
                'B': '会员积分',
                'C': '免费赠品',
                'D': '限时秒杀'
            }[strategy],
            'city': city,
            'clickthrough_rate': clickthrough_rates[i]
        })

# 转换为DataFrame并保存
df_exercise1 = pd.DataFrame(data_exercise1)
df_exercise1.to_csv(os.path.join(output_dir, 'marketing_strategy.csv'), index=False)

print(f"已生成练习1数据集：{len(df_exercise1)}行")

# =====================================================
# 练习2：区域与季节对销售的影响
# =====================================================
"""
某连锁零售商收集了不同区域（北区、南区、东区、西区）在不同季节（春、夏、秋、冬）的销售数据。
"""

# 设定基本销售额
base_sales = 1000

# 区域效应（主效应）
region_effects = {
    '北区': 200,
    '南区': 0,
    '东区': 150,
    '西区': 50
}

# 季节效应（主效应）
season_effects = {
    '春': 100,
    '夏': 200,
    '秋': 50,
    '冬': -50
}

# 交互效应（某些区域在特定季节表现特别好/差）
interaction_effects = {
    ('北区', '冬'): 300,  # 北区在冬季销售特别好
    ('南区', '夏'): 400,  # 南区在夏季销售特别好
    ('东区', '春'): 150,  # 东区在春季有额外提升
    ('西区', '秋'): 100   # 西区在秋季有额外提升
}

# 创建数据框
data_exercise2 = []

# 每个区域-季节组合有5次观测
observations_per_group = 5

for region in region_effects.keys():
    for season in season_effects.keys():
        # 计算此区域和季节组合的预期销售额
        expected_sales = base_sales + region_effects[region] + season_effects[season]
        
        # 添加交互效应（如果存在）
        if (region, season) in interaction_effects:
            expected_sales += interaction_effects[(region, season)]
        
        # 生成包含随机波动的销售数据
        sales_data = np.random.normal(expected_sales, expected_sales * 0.1, observations_per_group)
        
        for i in range(observations_per_group):
            data_exercise2.append({
                'region': region,
                'season': season,
                'store_id': f'{region}-{i+1}',  # 为每个区域创建5个不同的商店
                'sales': max(0, sales_data[i])  # 确保销售额非负
            })

# 转换为DataFrame并保存
df_exercise2 = pd.DataFrame(data_exercise2)
df_exercise2.to_csv(os.path.join(output_dir, 'regional_sales.csv'), index=False)

print(f"已生成练习2数据集：{len(df_exercise2)}行")

# =====================================================
# 练习3：广告媒体效果分析
# =====================================================
"""
分析不同广告媒体（电视、广播、社交媒体、印刷媒体）对不同产品类别（电子产品、服装、食品）的广告效果数据。
"""

# 设定基准效果（例如，转化率或ROI）
base_effect = 5.0

# 媒体主效应
media_effects = {
    '电视': 2.0,
    '广播': 0.5,
    '社交媒体': 1.5,
    '印刷媒体': 0.0
}

# 产品类别主效应
product_effects = {
    '电子产品': 1.0,
    '服装': 0.5,
    '食品': 0.0
}

# 交互效应（某些媒体对特定产品类别特别有效）
interaction_effects = {
    ('电视', '电子产品'): 1.5,       # 电视广告对电子产品特别有效
    ('社交媒体', '服装'): 2.0,       # 社交媒体对服装特别有效
    ('印刷媒体', '食品'): 1.0,       # 印刷媒体对食品相对有效
    ('广播', '食品'): 0.8           # 广播对食品有一定效果
}

# 创建数据框
data_exercise3 = []

# 每个媒体-产品组合有8次观测
observations_per_group = 8

for media in media_effects.keys():
    for product in product_effects.keys():
        # 计算此媒体和产品组合的预期效果
        expected_effect = base_effect + media_effects[media] + product_effects[product]
        
        # 添加交互效应（如果存在）
        if (media, product) in interaction_effects:
            expected_effect += interaction_effects[(media, product)]
        
        # 生成包含随机波动的效果数据
        effect_data = np.random.normal(expected_effect, 0.7, observations_per_group)
        
        for i in range(observations_per_group):
            data_exercise3.append({
                'media': media,
                'product_category': product,
                'campaign_id': f'{media[0]}{product[0]}-{i+1}',  # 为每个组合创建唯一的广告活动ID
                'effect': max(0, effect_data[i])  # 确保效果值非负
            })

# 转换为DataFrame并保存
df_exercise3 = pd.DataFrame(data_exercise3)
df_exercise3.to_csv(os.path.join(output_dir, 'ad_media_effect.csv'), index=False)

print(f"已生成练习3数据集：{len(df_exercise3)}行")

print("所有数据集生成完成！") 