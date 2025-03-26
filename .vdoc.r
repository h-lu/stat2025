#
#
#
#
#
#
library(tidyverse)
library(showtext)
library(RColorBrewer)
library(patchwork)
library(pacman)

# # 加载中文字体
# font_add_google("Noto Sans SC", "notosans")  # 思源黑体
# showtext_auto()

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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 加载波士顿房价数据集
library(MASS)
data(Boston)

# 查看数据集结构
str(Boston)

# 数据集概要
summary(Boston)

# 探索性数据分析：房价与几个主要因素之间的关系
p1 <- ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "房间数与房价关系",
       x = "平均房间数",
       y = "房价（千美元）")

p2 <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "低收入人口比例与房价关系",
       x = "低收入人口比例(%)",
       y = "房价（千美元）")

p3 <- ggplot(Boston, aes(x = crim, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "犯罪率与房价关系",
       x = "犯罪率",
       y = "房价（千美元）")

p1 + p2 + p3
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 以rm(房间数)与medv(房价)为例应用最小二乘法
# 计算均值
x_mean <- mean(Boston$rm)
y_mean <- mean(Boston$medv)

# 计算β1（斜率）
numerator <- sum((Boston$rm - x_mean) * (Boston$medv - y_mean))
denominator <- sum((Boston$rm - x_mean)^2)
beta1 <- numerator / denominator

# 计算β0（截距）
beta0 <- y_mean - beta1 * x_mean

# 展示计算结果
cat("手动计算的最小二乘估计值：\n")
cat("斜率(β1) =", beta1, "\n")
cat("截距(β0) =", beta0, "\n")

# 使用lm函数验证结果
model <- lm(medv ~ rm, data = Boston)
cat("\nlm函数计算的结果：\n")
print(coef(model))

# 绘制回归线
ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = beta0, slope = beta1, color = "red", size = 1.2) +
  labs(title = "波士顿房价与平均房间数的关系",
       subtitle = paste0("回归方程: 房价 = ", round(beta0, 2), " + ", round(beta1, 2), " × 房间数"),
       x = "平均房间数(rm)",
       y = "房价中位数(medv，千美元)") +
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
# 非线性关系示例：lstat与房价的关系更像指数关系
# 比较线性模型与对数变换模型
model_linear <- lm(medv ~ lstat, data = Boston)
model_log <- lm(medv ~ log(lstat), data = Boston)

# 创建预测值
Boston$pred_linear <- predict(model_linear)
Boston$pred_log <- predict(model_log)

# 可视化比较
p1 <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_linear), color = "red", size = 1) +
  labs(title = "线性模型",
       x = "低收入人口比例(%)",
       y = "房价(千美元)")

p2 <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_log), color = "blue", size = 1) +
  labs(title = "对数变换模型",
       x = "低收入人口比例(%)",
       y = "房价(千美元)")

p1 + p2

# 比较模型拟合度
cat("线性模型 R² =", summary(model_linear)$r.squared, "\n")
cat("对数变换模型 R² =", summary(model_log)$r.squared, "\n")
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
# 检测异方差性
plot(model_linear, which = 1)  # 残差图

# 进行BP检验
# 原假设H0：误差项具有同方差性
# 备择假设H1：误差项不具有同方差性（存在异方差性）
library(lmtest)
bptest(model_linear)

# 使用稳健标准误差
# 当模型存在异方差性时，普通最小二乘法（OLS）系数的标准误差估计会失效，导致推断不可靠。
# 稳健标准误差提供了一种有效的方法来估计系数的标准误差，即使存在异方差性。
# 在R中，`sandwich` 包的 `vcovHC` 函数可以计算稳健的协方差矩阵，然后与 `lmtest` 包的 `coeftest` 函数结合使用，进行基于稳健标准误差的系数检验。
# HC1 是一种常用的稳健标准误差类型。
library(sandwich)
coeftest(model_linear, vcov = vcovHC(model_linear, type = "HC1"))

# 加权最小二乘法示例：当误差项方差不齐时，WLS通过赋予不同观测点不同的权重来优化模型，通常权重与方差成反比。
# 计算残差的绝对值
abs_resid <- abs(residuals(model_linear))
# 基于预测值拟合残差模型
weight_model <- lm(abs_resid ~ fitted(model_linear))
# 计算权重
weights <- 1 / (fitted(weight_model)^2)
# 拟合WLS模型
wls_model <- lm(medv ~ lstat, data = Boston, weights = weights)

# 比较结果
summary(model_linear)$coefficients
summary(wls_model)$coefficients
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
# 创建一个人为的自相关序列
set.seed(123)
n <- 100
time <- 1:n
trend <- 0.5 * time
error <- arima.sim(list(ar = 0.8), n)
y <- trend + error
data_ts <- data.frame(time = time, y = y)

# 拟合简单线性模型
ts_model <- lm(y ~ time, data = data_ts)

# 检测自相关性
library(lmtest)
# Durbin-Watson检验
# 原假设H0：误差项不存在自相关性
# 备择假设H1：误差项存在自相关性
dwtest(ts_model)  

# 可视化残差
par(mfrow = c(1, 2))
plot(residuals(ts_model), type = "l", main = "残差时间序列")
acf(residuals(ts_model), main = "残差自相关函数")

# 使用广义最小二乘法纠正自相关
library(nlme)
gls_model <- gls(y ~ time, data = data_ts, correlation = corAR1())
summary(gls_model)

# 重置图形参数
par(mfrow = c(1, 1))
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
# 检验残差正态性
shapiro.test(residuals(model_linear))

# 残差QQ图
qqnorm(residuals(model_linear))
qqline(residuals(model_linear))

# Box-Cox变换
library(MASS)
bc <- boxcox(model_linear)
lambda <- bc$x[which.max(bc$y)]
cat("最优Box-Cox变换参数λ =", lambda, "\n")

# 应用Box-Cox变换
if(abs(lambda) < 0.001) {
  Boston$trans_medv <- log(Boston$medv)  # λ接近0时使用对数变换
} else {
  Boston$trans_medv <- (Boston$medv^lambda - 1) / lambda
}

# 拟合变换后的模型
trans_model <- lm(trans_medv ~ lstat, data = Boston)
shapiro.test(residuals(trans_model))

# 稳健回归
library(MASS)
robust_model <- rlm(medv ~ lstat, data = Boston)
summary(robust_model)
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
# 多元回归示例
multi_model <- lm(medv ~ rm + lstat + crim + nox, data = Boston)

# 检测多重共线性
library(car)
vif(multi_model)  # VIF > 10表示严重的多重共线性

# 相关矩阵
cor_matrix <- cor(Boston[, c("rm", "lstat", "crim", "nox")])
print(cor_matrix)

# 岭回归示例
library(MASS)
ridge_model <- lm.ridge(medv ~ rm + lstat + crim + nox, data = Boston, lambda = seq(0, 1, 0.01))
plot(ridge_model)  # 绘制岭迹图

# 选择最优lambda值
optimal_lambda <- ridge_model$lambda[which.min(ridge_model$GCV)]
cat("最优岭参数λ =", optimal_lambda, "\n")
ridge_coef <- coef(ridge_model)[which(ridge_model$lambda == optimal_lambda), ]
print(ridge_coef)
```
#
#
#
#
#
#
#
# 以rm(房间数)预测房价为例
# 建立简单线性回归模型
lm_model <- lm(medv ~ rm, data = Boston)

# 使用 broom 包获取模型摘要信息
library(broom)

# 获取整洁的模型系数
tidy(lm_model, conf.int = TRUE)

# 获取整洁的模型摘要统计量
glance(lm_model)

# 模型预测
new_data <- tibble(rm = c(5, 6, 7, 8))

# 使用 augment 获取带预测值和残差的数据框
augmented_data <- augment(lm_model, newdata = new_data, interval = "confidence")
augmented_prediction <- augment(lm_model, newdata = new_data, interval = "prediction")
augmented_data
augmented_prediction

# 回归诊断图
# 使用 ggfortify 包可以绘制更美观的回归诊断图，它是 ggplot2 的扩展包
library(ggfortify)
autoplot(lm_model)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 使用波士顿房价数据集进行变量选择示例
library(MASS)
data(Boston)

# 构建包含所有变量的完整模型
full_model <- lm(medv ~ ., data = Boston)
summary(full_model)
#
#
#
#
#
#
#
#
#
# 向前逐步回归
null_model <- lm(medv ~ 1, data = Boston)  # 仅含截距的模型
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                    direction = "forward", trace = FALSE)

# 查看最终模型
summary(forward_model)

# 查看选择过程
step(null_model, scope = list(lower = null_model, upper = full_model), 
    direction = "forward", trace = TRUE, steps = 2)  # 仅显示前两步以节省空间
#
#
#
#
#
#
#
#
#
# 向后逐步回归
backward_model <- step(full_model, direction = "backward", trace = FALSE)

# 查看最终模型
summary(backward_model)

# 比较向前和向后方法的结果
cat("向前逐步回归选择的变量：", 
    paste(names(coef(forward_model))[-1], collapse = ", "), "\n")
cat("向后逐步回归选择的变量：", 
    paste(names(coef(backward_model))[-1], collapse = ", "), "\n")
#
#
#
#
#
#
#
#
#
# 双向逐步回归
stepwise_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                      direction = "both", trace = FALSE)

# 查看最终模型
summary(stepwise_model)
#
#
#
#
#
#
#
#
#
#
#
# 使用leaps包进行最优子集选择
library(leaps)

# 执行最优子集选择
subsets <- regsubsets(medv ~ ., data = Boston, nvmax = 13)  # 最多考虑13个变量
subset_summary <- summary(subsets)

# 比较不同标准下的最佳模型
par(mfrow = c(2, 2))
plot(subset_summary$rss, xlab = "变量数量", ylab = "RSS", type = "b")
plot(subset_summary$adjr2, xlab = "变量数量", ylab = "调整R²", type = "b")
points(which.max(subset_summary$adjr2), subset_summary$adjr2[which.max(subset_summary$adjr2)], 
       col = "red", cex = 2, pch = 20)
plot(subset_summary$cp, xlab = "变量数量", ylab = "Cp", type = "b")
points(which.min(subset_summary$cp), subset_summary$cp[which.min(subset_summary$cp)], 
       col = "red", cex = 2, pch = 20)
plot(subset_summary$bic, xlab = "变量数量", ylab = "BIC", type = "b")
points(which.min(subset_summary$bic), subset_summary$bic[which.min(subset_summary$bic)], 
       col = "red", cex = 2, pch = 20)
par(mfrow = c(1, 1))

# 查看各标准下的最佳模型
cat("调整R²最大的模型包含", which.max(subset_summary$adjr2), "个变量\n")
cat("Cp最小的模型包含", which.min(subset_summary$cp), "个变量\n")
cat("BIC最小的模型包含", which.min(subset_summary$bic), "个变量\n")

# 展示BIC最小模型的变量
coef(subsets, which.min(subset_summary$bic))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
