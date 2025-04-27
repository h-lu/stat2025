#
#
#
#
#
#
#
#
#
#
#
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
eval_data <-augment(default_model, type.predict = "response")

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
    # 方法1: 使用基础 table() 函数
    # 注意：第一个参数是实际值，第二个参数是预测值
    confusion_matrix_base <- table(Predicted = eval_data$.fitted > 0.5, Observed = eval_data$default)
    
    print(confusion_matrix_base)
    
    # 方法2: 使用 yardstick 包 (更推荐，输出更规范)
    conf_mat_tbl <- eval_data %>%
      mutate(predicted_class = ifelse(.fitted > 0.5, "Yes", "No") %>% factor(levels = c("No", "Yes"))) %>%
      conf_mat(truth = default, estimate = predicted_class) # truth=真实值, estimate=预测值

    print(conf_mat_tbl)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
        # 手动计算
        accuracy_manual <- (9628 + 105) / (9628 + 105 + 228 + 39)
        print(accuracy_manual) # 约 0.9628

        # 使用 yardstick
        eval_data %>% accuracy(truth = observed, estimate = predicted_class)
        ```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
