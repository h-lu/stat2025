#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
    # 准备数据，确保 am 是因子或 0/1
    mtcars_data <- mtcars %>%
      mutate(am = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual"))) # 将 am 转为因子

    # glimpse(mtcars_data)

    # 拟合 Logistic 回归模型
    # 预测 am 为 Manual (1) 的概率
    logistic_model <- glm(am ~ hp + wt, data = mtcars_data, family = binomial)

    # 查看模型摘要
    summary(logistic_model)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
        library(broom)
        tidy(logistic_model)
        # 计算系数的 OR
        exp(coef(logistic_model))
        # (Intercept)          hp          wt
        # 199087.1037      1.0362      0.0022

        # 计算系数的置信区间 (对数优势尺度)
        confint(logistic_model)
        # 计算 OR 的置信区间
        exp(confint(logistic_model))
        #                    2.5 %      97.5 %
        # (Intercept) 3.900118e+00 1.616111e+07
        # hp          9.974981e-01 1.077501e+00
        # wt          1.760519e-04 2.203618e-01
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
