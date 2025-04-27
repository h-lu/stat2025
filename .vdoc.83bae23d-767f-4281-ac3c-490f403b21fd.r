#
#
#
#
#
#
#
#
#
#
#
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
library(broom)
library(pROC) # 用于 ROC 曲线和 AUC 计算
# install.packages("pROC") # 如果尚未安装
library(yardstick) # tidyverse 风格的模型评估包 (可选)
# install.packages("yardstick") # 如果尚未安装

# 回顾上周的模型
mtcars_data <- mtcars %>%
  mutate(am = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual")))

logistic_model <- glm(am ~ hp + wt, data = mtcars_data, family = binomial)
# summary(logistic_model)

# 获取模型预测的概率 (预测为 "Manual" 的概率)
predictions_prob <- predict(logistic_model, type = "response") # type="response" 返回概率

# 为了计算混淆矩阵等指标，需要将概率转换为类别预测
# 通常使用 0.5 作为阈值 (threshold)
threshold <- 0.5
predictions_class <- ifelse(predictions_prob > threshold, "Manual", "Automatic")
predictions_class <- factor(predictions_class, levels = c("Automatic", "Manual")) # 确保因子水平一致

# 将真实值和预测值放入一个 tibble，方便后续计算
eval_data <- tibble(
  observed = mtcars_data$am,
  predicted_prob = predictions_prob,
  predicted_class = predictions_class
)

# glimpse(eval_data)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
