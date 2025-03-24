library(tidyverse)
library(car)
# 使用tidyverse读取 marketing_strategy.csv 数据，
marketing_strategy <- read_csv("marketing_strategy.csv")
marketing_strategy

# 对不同策略 strategy 进行分组, 对每个组进行正态性检验
marketing_strategy %>%
  group_by(strategy) %>%
  summarise(
    # 进行正态性检验
    pvalue = shapiro.test(clickthrough_rate)$p.value
  )

# 再进行方差齐性检验
leveneTest(clickthrough_rate ~ strategy, data = marketing_strategy)

# 单因素方差分析
marketing_aov <- aov(clickthrough_rate ~ strategy, data = marketing_strategy)
summary(marketing_aov)

# 进行事后多重比较
TukeyHSD(marketing_aov)

marketing_strategy %>%
  distinct(strategy, strategy_name)
