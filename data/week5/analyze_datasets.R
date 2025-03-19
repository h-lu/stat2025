# 分析生成的数据集
library(tidyverse)
library(car)  # 用于方差分析

# =====================================================
# 练习1：市场营销策略评估
# =====================================================
cat("\n========== 练习1：市场营销策略评估 ==========\n")

# 读取数据
marketing_data <- read.csv("marketing_strategy.csv")

# 数据概览
cat("数据概览:\n")
str(marketing_data)
summary(marketing_data)

# 检查四种策略的点击率描述性统计
cat("\n各策略点击率描述性统计:\n")
group_stats <- marketing_data %>%
  group_by(strategy, strategy_name) %>%
  summarise(
    平均点击率 = mean(clickthrough_rate),
    标准差 = sd(clickthrough_rate),
    最小值 = min(clickthrough_rate),
    最大值 = max(clickthrough_rate),
    样本数 = n()
  )
print(group_stats)

# 方差分析
cat("\n单因素方差分析结果:\n")
aov_result <- aov(clickthrough_rate ~ strategy, data = marketing_data)
summary(aov_result)

# 如果方差分析显示显著差异，进行多重比较
cat("\nTukey多重比较结果:\n")
tukey_result <- TukeyHSD(aov_result)
print(tukey_result)

# 检查方差齐性假设
cat("\n检验方差齐性 (Levene's Test):\n")
levene_test <- leveneTest(clickthrough_rate ~ strategy, data = marketing_data)
print(levene_test)

# =====================================================
# 练习2：区域与季节对销售的影响
# =====================================================
cat("\n\n========== 练习2：区域与季节对销售的影响 ==========\n")

# 读取数据
regional_data <- read.csv("regional_sales.csv")

# 数据概览
cat("数据概览:\n")
str(regional_data)
summary(regional_data)

# 查看区域和季节的交叉表
cat("\n区域和季节的观测数量交叉表:\n")
region_season_counts <- table(regional_data$region, regional_data$season)
print(region_season_counts)

# 描述性统计
cat("\n各区域各季节销售额描述性统计:\n")
group_stats2 <- regional_data %>%
  group_by(region, season) %>%
  summarise(
    平均销售额 = mean(sales),
    标准差 = sd(sales),
    样本数 = n()
  )
print(group_stats2)

# 双因素方差分析
cat("\n双因素方差分析结果 (包含交互效应):\n")
aov_result2 <- aov(sales ~ region * season, data = regional_data)
summary(aov_result2)

# 主效应模型
cat("\n双因素方差分析结果 (仅主效应):\n")
aov_result2_main <- aov(sales ~ region + season, data = regional_data)
summary(aov_result2_main)

# 检查方差齐性假设
cat("\n检验方差齐性 (Levene's Test):\n")
levene_test2 <- leveneTest(sales ~ interaction(region, season), data = regional_data)
print(levene_test2)

# =====================================================
# 练习3：广告媒体效果分析
# =====================================================
cat("\n\n========== 练习3：广告媒体效果分析 ==========\n")

# 读取数据
ad_data <- read.csv("ad_media_effect.csv")

# 数据概览
cat("数据概览:\n")
str(ad_data)
summary(ad_data)

# 查看媒体和产品类别的交叉表
cat("\n媒体和产品类别的观测数量交叉表:\n")
media_product_counts <- table(ad_data$media, ad_data$product_category)
print(media_product_counts)

# 描述性统计
cat("\n各媒体各产品类别效果描述性统计:\n")
group_stats3 <- ad_data %>%
  group_by(media, product_category) %>%
  summarise(
    平均效果 = mean(effect),
    标准差 = sd(effect),
    样本数 = n()
  )
print(group_stats3)

# 双因素方差分析
cat("\n双因素方差分析结果 (包含交互效应):\n")
aov_result3 <- aov(effect ~ media * product_category, data = ad_data)
summary(aov_result3)

# 主效应模型
cat("\n双因素方差分析结果 (仅主效应):\n")
aov_result3_main <- aov(effect ~ media + product_category, data = ad_data)
summary(aov_result3_main)

# 检查方差齐性假设
cat("\n检验方差齐性 (Levene's Test):\n")
levene_test3 <- leveneTest(effect ~ interaction(media, product_category), data = ad_data)
print(levene_test3)

# 对于有显著交互效应的情况，分析在不同产品类别下最有效的广告媒体
cat("\n对于有显著交互效应的情况，分析在不同产品类别下最有效的广告媒体:\n")
for (product in unique(ad_data$product_category)) {
  cat("\n产品类别:", product, "\n")
  product_data <- ad_data %>% filter(product_category == product)
  product_stats <- product_data %>%
    group_by(media) %>%
    summarise(
      平均效果 = mean(effect),
      标准差 = sd(effect)
    ) %>%
    arrange(desc(平均效果))
  print(product_stats)
  
  # 针对该产品类别进行单因素方差分析
  product_aov <- aov(effect ~ media, data = product_data)
  cat("方差分析结果:\n")
  print(summary(product_aov))
  
  # 如果有显著差异，进行多重比较
  if (summary(product_aov)[[1]]["media", "Pr(>F)"] < 0.05) {
    cat("Tukey多重比较结果:\n")
    product_tukey <- TukeyHSD(product_aov)
    print(product_tukey)
  }
} 