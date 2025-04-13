#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
library(ggplot2)
library(car)    # For vif() and leveneTest() if needed
library(broom)  # For augment()

# --- 模型 1: mpg 数据 ---
# mpg 数据集已在 tidyverse 中
mlr_hwy <- lm(hwy ~ displ + cyl + drv, data = mpg)
# summary(mlr_hwy) # 回顾模型

# --- 模型 2: 问题数据 ---
set.seed(42)
n_prob <- 100
x1_prob <- rnorm(n_prob)
x2_prob <- x1_prob * 0.8 + rnorm(n_prob, 0, 0.1) # 共线性
x3_prob <- rnorm(n_prob)
y_prob <- 2 + 3*x1_prob + 1*x2_prob - 2*x3_prob + rnorm(n_prob, 0, 2)
y_prob[1] <- y_prob[1] + 15 # 异常/强影响点
x1_prob[2] <- x1_prob[2] + 4 # 高杠杆点
prob_data <- tibble(y = y_prob, x1 = x1_prob, x2 = x2_prob, x3 = x3_prob)
prob_model <- lm(y ~ x1 + x2 + x3, data = prob_data)
# summary(prob_model) # 回顾模型
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| include: false
# 1. 对 mlr_hwy 进行诊断绘图
par(mfrow = c(2, 2)) # 设置 2x2 绘图区域
plot(mlr_hwy)
par(mfrow = c(1, 1)) # 恢复默认

# 2. 解读 Residuals vs Fitted (mlr_hwy):
print("--- mlr_hwy: Residuals vs Fitted 解读 ---")
print("红线略有弯曲，不是完全水平，提示可能存在轻微的非线性或异方差。")
print("点的散布宽度在拟合值较大时似乎略有增加，可能轻微违反等方差。")

# 3. 解读 Normal Q-Q (mlr_hwy):
print("--- mlr_hwy: Normal Q-Q 解读 ---")
print("大部分点落在直线上，但两端（特别是右尾）略有偏离，提示残差可能比正态分布的尾部更“重”一点。")
print("但对于大样本量，这种偏离可能不严重。")

# 4. 解读 Scale-Location (mlr_hwy):
print("--- mlr_hwy: Scale-Location 解读 ---")
print("红线略有上升趋势，再次提示可能存在轻微的异方差（方差随拟合值增大而增大）。")

# 5. 解读 Residuals vs Leverage (mlr_hwy):
print("--- mlr_hwy: Residuals vs Leverage 解读 ---")
print("大部分点的杠杆值较低。有少数点杠杆值稍高，但残差不大。")
print("没有明显的点落在 Cook's D 轮廓线之外，提示可能没有特别强的单一影响点。")

# 6. 对 prob_model 进行诊断绘图并比较
par(mfrow = c(2, 2))
plot(prob_model)
par(mfrow = c(1, 1))

print("--- prob_model: 诊断图解读 ---")
print("Residuals vs Fitted: 红线可能因异常点(点1)而弯曲，点 1 可能远离其他点。")
print("Normal Q-Q: 点 1 可能严重偏离直线。")
print("Scale-Location: 点 1 可能位置很高，红线可能受其影响。")
print("Residuals vs Leverage: 点 1 可能残差和 Cook's D 都很大（强影响点）。点 2 可能杠杆值很高，但残差不一定大。")

#
#
#
#
#
#
#
# 1. 提取 mlr_hwy 残差
residuals_hwy <- residuals(mlr_hwy)

# 2. 对 mlr_hwy 残差进行 Shapiro-Wilk 检验
shapiro_hwy <- shapiro.test(residuals_hwy)
print("Shapiro-Wilk Test for mlr_hwy residuals:")
print(shapiro_hwy)

# 3. 解读 mlr_hwy 结果
print("mlr_hwy 残差正态性解读:")
if (shapiro_hwy$p.value < 0.05) {
  print("Shapiro-Wilk 检验 P 值小于 0.05，拒绝正态性假设。")
  print("结合 Q-Q 图，虽然检验显著，但大样本下轻微偏离可能不严重影响结果。")
} else {
  print("Shapiro-Wilk 检验 P 值大于 0.05，未能拒绝正态性假设。")
}


# 4. 对 prob_model 残差进行检验
residuals_prob <- residuals(prob_model)
shapiro_prob <- shapiro.test(residuals_prob)
print("Shapiro-Wilk Test for prob_model residuals:")
print(shapiro_prob)
print("prob_model 残差正态性解读:")
if (shapiro_prob$p.value < 0.05) {
  print("Shapiro-Wilk 检验 P 值小于 0.05，拒绝正态性假设。这可能是由异常点引起的。")
} else {
  print("Shapiro-Wilk 检验 P 值大于 0.05，未能拒绝正态性假设。")
}
#
#
#
#
#
#
#
# 1. 计算 mlr_hwy 的 VIF
print("--- VIF for mlr_hwy ---")
vif_hwy <- vif(mlr_hwy)
print(vif_hwy)

# 2. 解读 mlr_hwy VIF
print("mlr_hwy VIF 解读:")
print("displ 和 cyl 的 VIF 可能较高（例如 > 5），因为排量和气缸数通常高度相关。")
print("这提示模型中存在一定的共线性，解释这两个变量的独立效应时需注意。")
print("drv 的 VIF 通常较低。")


# 3. 计算 prob_model 的 VIF 并比较
print("--- VIF for prob_model ---")
vif_prob <- vif(prob_model)
print(vif_prob)
print("prob_model VIF 解读:")
print("x1 和 x2 的 VIF 应该非常高（远大于 10），因为我们故意让它们高度相关。")
print("这证实了 prob_model 中存在严重的共线性问题。")

#
#
#
#
#
#
#
# 1. 计算 mlr_hwy 的 Cook's D
cooks_hwy <- cooks.distance(mlr_hwy)

# 2. 找出 mlr_hwy 的潜在影响点
n_hwy <- nrow(mpg)
cutoff_hwy <- 4 / n_hwy
influential_hwy_indices <- which(cooks_hwy > cutoff_hwy)
print(paste("Potential influential points in mlr_hwy (indices, cutoff=", round(cutoff_hwy, 4), "):"))
# print(influential_hwy_indices) # 输出可能较长
print(paste("Number of potential influential points in mlr_hwy:", length(influential_hwy_indices)))
print(paste("Max Cook's D in mlr_hwy:", round(max(cooks_hwy), 3)))
print("解读：可能有一些点超过阈值，但最大值可能不大，需要结合 Residuals vs Leverage 图判断。")


# 可视化 Cook's D
# plot(mlr_hwy, which = 4) # Cook's distance plot
# 或者
augment(mlr_hwy) %>%
  mutate(obs_index = row_number()) %>%
  ggplot(aes(x = obs_index, y = .cooksd)) +
  geom_col(fill = "skyblue", width = 0.7) +
  geom_hline(yintercept = cutoff_hwy, color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance Plot (mlr_hwy)", x = "Observation Index", y = "Cook's Distance") +
  theme_minimal()


# 3. 对 prob_model 进行 Cook's D 分析
cooks_prob <- cooks.distance(prob_model)
n_prob <- nrow(prob_data)
cutoff_prob <- 4 / n_prob
influential_prob_indices <- which(cooks_prob > cutoff_prob)
print(paste("Potential influential points in prob_model (indices, cutoff=", round(cutoff_prob, 4), "):"))
print(influential_prob_indices)
print(paste("Max Cook's D in prob_model:", round(max(cooks_prob), 3)))
print("解读：预期点 1 的 Cook's D 会很大。检查点 1 是否在索引中，并且其值是否远超其他点。")


# 查看 Cook's D 值
# print("Cook's D for prob_model:")
# print(round(cooks_prob, 3))
# 观察第一个值是否特别大

# plot(prob_model, which = 4) # Cook's distance plot for prob_model
augment(prob_model) %>%
  mutate(obs_index = row_number()) %>%
  ggplot(aes(x = obs_index, y = .cooksd)) +
  geom_col(fill = "salmon", width = 0.7) +
  geom_hline(yintercept = cutoff_prob, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0.5, color = "blue", linetype = "dotted") +
  geom_hline(yintercept = 1, color = "darkgreen", linetype = "dotted") +
  labs(title = "Cook's Distance Plot (prob_model)", x = "Observation Index", y = "Cook's Distance") +
  theme_minimal()

#
#
#
#
#
#
#
