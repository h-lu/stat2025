#
#
#
#
#
#
#
#
#
#
#
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
        print(recall_manual)

        # 使用 yardstick
        eval_data %>% 
            recall(
                truth = default,
                estimate = predict_class,
                event_level = "second" # "second" 表示第二个因子水平 "Yes" 是 positive 类
                )
        # 或者使用 sensitivity()
        eval_data %>% 
            sensitivity(
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
        # 手动计算
        f1_manual <- 2 * (precision_manual * recall_manual) / (precision_manual + recall_manual)
        print(f1_manual)

        # 使用 yardstick
        eval_data %>% 
            f_meas(
                truth = default,
                estimate = predict_class,
                event_level = "second" # "second" 表示第二个因子水平 "Yes" 是 positive 类
                ) # f_meas 计算 F-beta score, 默认 beta=1 即 F1
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
        # 手动计算
        specificity_manual <- 9628 / (9628 + 39)
        print(specificity_manual)

        # 使用 yardstick
        eval_data %>% 
            specificity(
                truth = default,
                estimate = predict_class,
                event_level = "second" # "second" 表示第二个因子水平 "Yes" 是 positive 类
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
    # 计算 AUC 值
    auc_value <- eval_data %>%
      yardstick::roc_auc(
        truth = default,
        .fitted, # 预测为 "Yes" 的概率列
        event_level = "second"
      )
    print(auc_value)

    # 计算 ROC 曲线数据
    roc_curve_data <- eval_data %>%
      yardstick::roc_curve(
        truth = default,
        .fitted, # 预测为 "Yes" 的概率列
        event_level = "second"
      )

    # 绘制 ROC 曲线
    ggplot(roc_curve_data, aes(x = 1 - specificity, y = sensitivity)) +
      geom_line(color = "blue", size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
      labs(
        title = paste0("ROC Curve (AUC = ", round(auc_value$.estimate, 3), ")"),
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)"
      ) +
      theme_minimal()

    # 找到最佳阈值（例如，Youden's J 最大化）
    best_threshold <- roc_curve_data %>%
      mutate(youden_j = sensitivity + specificity - 1) %>%
      filter(youden_j == max(youden_j)) %>%
      select(.threshold, sensitivity, specificity, youden_j)
    print(best_threshold)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
    # 假设我们有另一个模型，只用 wt 预测 am
    logistic_model_wt_only <- glm(am ~ wt, data = mtcars_data, family = binomial)

    # 比较两个模型的 AIC 和 BIC
    AIC(logistic_model, logistic_model_wt_only)
    BIC(logistic_model, logistic_model_wt_only)

    # 结果显示 logistic_model (包含 hp 和 wt) 的 AIC 和 BIC 都更小，
    # 表明在拟合优度和复杂度之间权衡后，它是相对更优的模型。
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
