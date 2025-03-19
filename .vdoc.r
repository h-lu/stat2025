#
#
#
#
#
library(tidyverse)
library(palmerpenguins)
library(showtext)
library(RColorBrewer)

# 加载中文字体
font_add_google("Noto Sans SC", "notosans")  # 思源黑体
showtext_auto()

# 设置ggplot2默认主题
theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "Noto Sans SC"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 创建示例数据
marketing_data <- data.frame(
  campaign = rep(c("A", "B", "C", "D"), each = 30),
  sales = c(rnorm(30, 100, 15), rnorm(30, 110, 18), 
            rnorm(30, 95, 20), rnorm(30, 115, 25))
)

# 绘制箱线图比较各营销活动的销售效果
ggplot(marketing_data, aes(x = campaign, y = sales, fill = campaign)) +
  geom_boxplot() +
  labs(title = "不同营销活动的销售效果比较",
       x = "营销活动", y = "销售额")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 正态性检验
#   - 原假设 (H0): 数据服从正态分布
#   - 备择假设 (H1): 数据不服从正态分布
marketing_data %>%
  group_by(campaign) %>%
  summarise(
    shapiro_p_value = shapiro.test(sales)$p.value # 使用 Shapiro-Wilk 检验正态性
  )

# 检验方差齐性
#   - 原假设 (H0): 各组别总体方差相等
#   - 备择假设 (H1): 至少有一组别总体方差与其他组别总体方差不相等
library(car)
leveneTest(sales ~ campaign, data = marketing_data) # Levene's 检验 - 检验方差齐性
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 单因素方差分析
aov_result <- aov(sales ~ campaign, data = marketing_data)
summary(aov_result)

# 如果方差分析结果表明组间存在显著差异，则需要进行多重比较，进一步分析哪些组别之间存在显著差异
# TukeyHSD (Tukey's Honestly Significant Differences) 函数用于执行Tukey事后多重比较，
# 它可以检验所有可能的组别配对之间的均值差异，并控制族错误率 (family-wise error rate)，
# 从而避免由于进行多次比较而增加犯第一类错误的概率。
TukeyHSD(aov_result)
#
#
#
#
# 可视化多重比较结果
tukey_result <- TukeyHSD(aov_result)
tukey_df <- as.data.frame(tukey_result$campaign)
tukey_df$comparison <- rownames(tukey_df)

ggplot(tukey_df, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Tukey多重比较结果",
       x = "组间比较", y = "均值差异")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 创建包含交互效应的示例数据
pricing_data <- data.frame(
  price_level = factor(rep(c("低", "中", "高"), each = 60), levels = c("低", "中", "高")),
  consumer_group = rep(rep(c("年轻人", "中年人"), each = 30), 3),
  purchase_amount = c(
    rnorm(30, 120, 20), rnorm(30, 100, 15),  # 低价格，两个消费群体
    rnorm(30, 100, 25), rnorm(30, 110, 20),  # 中价格，两个消费群体
    rnorm(30, 70, 30), rnorm(30, 130, 35)    # 高价格，两个消费群体
  )
)
#
#
#
#
#
#
#
#
#
#
# 双因素方差分析（包含交互效应）
interaction_model <- aov(purchase_amount ~ price_level * consumer_group, 
                         data = pricing_data)
summary(interaction_model)

# 不包含交互效应的模型
main_effects_model <- aov(purchase_amount ~ price_level + consumer_group, 
                          data = pricing_data)
summary(main_effects_model)

# 交互作用图
ggplot(pricing_data, aes(x = price_level, y = purchase_amount, 
                        color = consumer_group, group = consumer_group)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "价格水平与消费者群体的交互效应",
       x = "价格水平", y = "购买金额")
#
#
#
#
#
#
#
# 重复测量方差分析示例
library(ez)
repeated_data <- data.frame(
  subject = factor(rep(1:30, each = 3)),
  time = factor(rep(c("before", "during", "after"), 30)),
  performance = c(
    rnorm(30, 70, 10),   # before
    rnorm(30, 85, 12),   # during
    rnorm(30, 75, 15)    # after
  )
)

# 使用ezANOVA函数进行重复测量方差分析
ezANOVA(
  data = repeated_data,
  dv = .(performance),
  wid = .(subject),
  within = .(time),
  detailed = TRUE
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Kruskal-Wallis检验
kruskal.test(sales ~ campaign, data = marketing_data)
#
#
#
#
#
# 包含协变量的ANCOVA模型
ancova_model <- aov(sales ~ campaign + customer_loyalty, data = marketing_extended)
summary(ancova_model)

# 嵌套设计方差分析
nested_model <- aov(performance ~ treatment + Error(subject/time), 
                   data = nested_data)
summary(nested_model)

# 多变量方差分析(MANOVA)
manova_result <- manova(cbind(sales, satisfaction) ~ campaign, 
                        data = multi_response_data)
summary(manova_result)
#
#
#
#
#
#
#
#
#
# 读取营销策略数据
marketing_data <- read_csv("data/week5/marketing_strategy.csv")

# 查看数据
head(marketing_data)

# 绘制四种策略的点击率箱线图
ggplot(marketing_data, aes(x = strategy_name, y = clickthrough_rate, fill = strategy_name)) +
  geom_boxplot() +
  labs(title = "不同营销策略的点击率比较",
       x = "营销策略", y = "点击率") +
  theme(legend.position = "none")
#
#
#
#
#
#
#
#
#
#
#
#
# 读取区域销售数据
regional_data <- read_csv("data/week5/regional_sales.csv") %>%
  mutate(
    season = factor(season, levels = c("春", "夏", "秋", "冬")),
    region = factor(region, levels = c("东区", "南区",  "西区", "北区"))
  )

# 查看数据
head(regional_data)

# 计算每个区域和季节组合的平均销售额
region_season_means <- regional_data %>%
  group_by(region, season) %>%
  summarise(mean_sales = mean(sales), .groups = "drop")

# 绘制交互作用图
ggplot(region_season_means, aes(x = season, y = mean_sales, 
                               color = region, group = region)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "区域与季节对销售的交互效应",
       x = "季节", y = "平均销售额") +
  scale_color_brewer(palette = "Set1")
#
#
#
#
#
#
#
#
#
#
#
#
# 读取广告媒体效果数据
ad_data <- read_csv("data/week5/ad_media_effect.csv")

# 查看数据
head(ad_data)

# 计算每个媒体和产品类别组合的平均效果
media_product_means <- ad_data %>%
  group_by(media, product_category) %>%
  summarise(mean_effect = mean(effect), .groups = "drop")

# 绘制交互作用图
ggplot(media_product_means, aes(x = product_category, y = mean_effect, 
                               color = media, group = media)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "广告媒体与产品类别的交互效应",
       x = "产品类别", y = "平均效果") +
  scale_color_brewer(palette = "Set2")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
