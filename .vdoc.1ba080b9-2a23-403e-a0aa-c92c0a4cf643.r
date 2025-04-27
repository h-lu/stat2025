#
#
#
#
#
#
#
#
#
#
#
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
eval_data <-augment(default_model, type.predict = "response") %>%
  mutate(predict_class = as.factor(ifelse(.fitted > 0.5, "Yes", "No")))

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
    confusion_matrix_base <- table(Predicted = eval_data$predict_class, Observed = eval_data$default)
    
    print(confusion_matrix_base)
    
    # 方法2: 使用 yardstick 包 (更推荐，输出更规范)
    conf_mat_tbl <- eval_data %>%
      conf_mat(truth = default, estimate = predict_class) # truth=真实值, estimate=预测值

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
        print(accuracy_manual)

        # 使用 yardstick
        eval_data %>% accuracy(truth = default, estimate = predict_class)
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
        # 手动计算 (预测为 Manual 的精确率)
        precision_manual <- 105 / (105 + 39)
        print(precision_manual)

        # 使用 yardstick (需要指定 positive 类)
        eval_data %>% 
            precision(
                truth = default,
                estimate = predict_class,
                event_level = "second" # "second" 表示第二个因子水平 "Yes" 是 positive 类
                )
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
        # 手动计算 (Manual 类的召回率)
        recall_manual <- 105 / (105 + 228)
        print(recall_manual) # 约 0.769 (在这个例子中恰好与 Precision 相等)

        # 使用 yardstick
        eval_data %>% recall(truth = observed, estimate = predicted_class, event_level = "second")
        # 或者使用 sensitivity()
        eval_data %>% sensitivity(truth = observed, estimate = predicted_class, event_level = "second")
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
