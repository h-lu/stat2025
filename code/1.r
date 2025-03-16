library(tidyverse)

student_scores = list(
    before = c(68, 72, 65, 82, 75, 70, 64, 83, 76, 79, 71, 74),
    after = c(73, 76, 70, 85, 79, 75, 69, 88, 81, 84, 74, 79),
    description = "学习干预前后的学生成绩"
  )

student_scores

#H0: 干预前后学生成绩没有显著差异
#H1: 干预前后学生成绩有显著差异

# 配对双样本均值假设检验公式

# 计算每个学生干预前后的差值, 对差值进行单样本t检验

# 计算每个学生干预前后的差值
diff = student_scores$after - student_scores$before

# 对差值进行单样本t检验
t.test(diff, mu = 0)

# 手工计算 p值
t = mean(diff) / (sd(diff) / sqrt(length(diff)))
p = 2*pt(abs(t), df = length(diff) - 1, lower.tail = FALSE)
p

  movie_ratings = list(
    action = c(4.5, 4.2, 3.8, 4.7, 4.0, 4.3, 3.9, 4.6, 4.1, 4.4),
    comedy = c(3.8, 4.1, 3.6, 4.2, 4.0, 3.9, 4.3, 4.0, 3.7, 4.5),
    description = "电影评分数据（1-5分）"
  )

movie_ratings

#H0: 动作片和喜剧片的评分没有显著差异
#H1: 动作片和喜剧片的评分有显著差异

# 独立双样本均值假设检验公式
# t = (x̄1 - x̄2) / sqrt((s1^2 / n1) + (s2^2 / n2))

# 计算动作片和喜剧片的均值
action_mean = mean(movie_ratings$action)
comedy_mean = mean(movie_ratings$comedy)

# 计算动作片和喜剧片的标准差
action_sd = sd(movie_ratings$action)
comedy_sd = sd(movie_ratings$comedy)

# 计算动作片和喜剧片的样本量
action_n = length(movie_ratings$action)
comedy_n = length(movie_ratings$comedy)

# 计算t值
t = (action_mean - comedy_mean) / sqrt((action_sd^2 / action_n) + (comedy_sd^2 / comedy_n))
t

# 计算p值
p = 2*pt(abs(t), df = action_n + comedy_n - 2, lower.tail = FALSE)
p

t.test(movie_ratings$action, movie_ratings$comedy, paired = FALSE, var.equal = T)


before <- c(82.5, 79.3, 88.7, 73.2, 85.6, 92.4, 77.8, 83.1, 90.5, 76.3)
after <- c(78.9, 76.2, 85.1, 70.8, 81.9, 88.5, 75.2, 80.0, 86.7, 73.5 )
t.test(before, after, paired = TRUE)

library(pwr)
pwr.t.test(n = 10, power = 0.8, sig.level = 0.05, type = "paired", alternative = "two.sided")




