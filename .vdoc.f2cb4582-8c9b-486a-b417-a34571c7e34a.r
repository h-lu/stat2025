#
#
#
#
#
#
#
#
#
#
#
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
library(pROC) # 用于 ROC 曲线和 AUC 计算
# install.packages("pROC") # 如果尚未安装
library(yardstick) # tidyverse 风格的模型评估包 (可选)
# install.packages("yardstick") # 如果尚未安装

# 加载 Default 数据集
data(Default)
Default <- ISLR::Default %>%
  mutate(
    default = factor(default, levels = c("No", "Yes")),
    student = factor(student)
  )

# 拟合 Logistic 回归模型
default_model <- glm(default ~ balance + student, data = Default, family = binomial)
# summary(default_model)

# 获取模型预测的概率 (预测为 "Yes" 的概率)
augment(default_model, type.predict = "response")

predictions_prob <- predict(default_model, type = "response") # type="response" 返回概率

# 为了计算混淆矩阵等指标，需要将概率转换为类别预测
# 通常使用 0.5 作为阈值 (threshold)
threshold <- 0.5
predictions_class <- ifelse(predictions_prob > threshold, "Yes", "No")
predictions_class <- factor(predictions_class, levels = c("No", "Yes")) # 确保因子水平一致

# 将真实值和预测值放入一个 tibble，方便后续计算
eval_data <- tibble(
  observed = Default$default,
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
