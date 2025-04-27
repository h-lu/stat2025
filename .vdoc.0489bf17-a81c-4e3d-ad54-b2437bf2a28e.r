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
#| code-fold: true
library(tidyverse)
# 生成一系列 z 值
z <- seq(-10, 10, length.out = 100)

# 计算对应的概率 p
p <- 1 / (1 + exp(-z))

# 绘制 Sigmoid 函数图像
ggplot(data.frame(z = z, p = p), aes(x = z, y = p)) +
  geom_line(color = "blue") +
  labs(x = "z", y = "p(z)", title = "Sigmoid Function") +
  theme_minimal()
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
    library(tidyverse)
    library(ISLR)
    library(broom)

    # 1. 数据准备：确保 default 是因子，student 也是因子
    data("Default")
    glimpse(Default)
    # 默认 default 已为因子，student 也是因子

    # 2. 拟合 Logistic 回归模型
    logistic_model <- glm(default ~ balance + student, data = Default, family = binomial)

    # 3. 查看模型摘要（系数、标准误、z值、P值等）
    tidy_logistic <- tidy(logistic_model)
    print(tidy_logistic)

    # 4. 计算系数的优势比（OR）及其置信区间
    # 4.1 计算 OR
    or_table <- tidy_logistic %>%
      mutate(OR = exp(estimate))
    print(or_table)

    # 4.2 计算置信区间（对数优势尺度）
    confint_logistic <- confint(logistic_model)
    print(confint_logistic)

    # 4.3 计算 OR 的置信区间
    or_ci <- exp(confint_logistic)
    print(or_ci)
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
