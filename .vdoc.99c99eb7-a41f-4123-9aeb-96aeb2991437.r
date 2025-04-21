#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
library(car)    # For vif()
library(broom)  # For augment()
library(interactions) # For interact_plot()

# --- 模型 1: mpg 数据 ---
mlr_hwy <- lm(hwy ~ displ + cyl + drv, data = mpg)
# summary(mlr_hwy)
# vif(mlr_hwy)
# par(mfrow=c(2,2)); plot(mlr_hwy); par(mfrow=c(1,1))


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
# summary(prob_model)
# vif(prob_model)
# plot(prob_model, which=4) # Cook's D
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
