# 全局设置和数据
# 包含应用程序中需要共享的变量和数据

# 中文字体设置
font_add_google("Noto Sans SC", "notosans")  # 思源黑体
font_add_google("Roboto", "roboto")         # 添加Roboto字体，更现代
showtext_auto()

# 配色设置 - 现代风格配色
app_colors <- list(
  # 使用预定义名称，保持与shinydashboard兼容
  primary = "light-blue",    # 主要颜色 (浅蓝色)
  secondary = "purple",      # 次要颜色
  success = "green",         # 成功颜色
  warning = "yellow",        # 警告颜色
  danger = "red",            # 危险颜色
  info = "aqua",             # 信息颜色
  
  # 用于图表和其他UI元素的精确颜色
  chart_primary = "#3498db",   # 浅蓝色，更现代的蓝色
  chart_secondary = "#9b59b6", # 紫色
  chart_success = "#2ecc71",   # 翠绿色
  chart_warning = "#f1c40f",   # 明黄色
  chart_danger = "#e74c3c",    # 红色
  chart_info = "#1abc9c",      # 青绿色
  chart_gray = "#95a5a6",      # 灰色
  
  # 背景和文字颜色
  bg_light = "#f8f9fa",       # 浅色背景
  bg_dark = "#343a40",        # 深色背景
  text_dark = "#2c3e50",      # 深色文字
  text_light = "#ecf0f1"      # 浅色文字
)

# 样本数据示例
sample_data <- list(
  # 电影评分数据
  movie_ratings = list(
    action = c(4.5, 4.2, 3.8, 4.7, 4.0, 4.3, 3.9, 4.6, 4.1, 4.4),
    comedy = c(3.8, 4.1, 3.6, 4.2, 4.0, 3.9, 4.3, 4.0, 3.7, 4.5),
    description = "电影评分数据（1-5分）"
  ),
  
  # 学生成绩数据
  student_scores = list(
    before = c(68, 72, 65, 82, 75, 70, 64, 83, 76, 79, 71, 74),
    after = c(73, 76, 70, 85, 79, 75, 69, 88, 81, 84, 74, 79),
    description = "学习干预前后的学生成绩"
  ),
  
  # 两种药物治疗效果数据
  treatment_effect = list(
    drug_a = c(12, 15, 10, 18, 14, 16, 11, 13, 17, 15, 14, 16, 13, 15),
    drug_b = c(14, 18, 16, 21, 17, 19, 15, 16, 20, 18, 17, 19, 16, 18),
    description = "两种药物的治疗效果指标"
  ),
  
  # 减肥计划数据
  weight_loss = list(
    before = c(82.5, 79.3, 88.7, 73.2, 85.6, 92.4, 77.8, 83.1, 90.5, 76.3),
    after = c(78.9, 76.2, 85.1, 70.8, 81.9, 88.5, 75.2, 80.0, 86.7, 73.5),
    description = "减肥计划前后的体重（公斤）"
  )
)

# 预定义假设检验场景
hypothesis_scenarios <- list(
  list(
    title = "场景一：电影平均评分",
    description = "某电影类型的平均评分是否高于4.0分？",
    null_hypothesis = "H₀: μ = 4.0（平均评分等于4.0）",
    alternative_hypothesis = "H₁: μ > 4.0（平均评分高于4.0）",
    test_type = "单样本t检验（右尾）"
  ),
  list(
    title = "场景二：新旧教学方法比较",
    description = "新教学方法是否比旧教学方法更有效？",
    null_hypothesis = "H₀: μ₁ - μ₂ = 0（两种方法效果相同）",
    alternative_hypothesis = "H₁: μ₁ - μ₂ > 0（新方法比旧方法更有效）",
    test_type = "独立样本t检验（右尾）"
  ),
  list(
    title = "场景三：减肥计划效果",
    description = "减肥计划前后体重是否有显著差异？",
    null_hypothesis = "H₀: μd = 0（减肥前后体重无差异）",
    alternative_hypothesis = "H₁: μd < 0（减肥后体重显著降低）",
    test_type = "配对样本t检验（左尾）"
  ),
  list(
    title = "场景四：药物副作用",
    description = "某药物是否会导致血压变化？",
    null_hypothesis = "H₀: μd = 0（用药前后血压无差异）",
    alternative_hypothesis = "H₁: μd ≠ 0（用药前后血压有显著差异）",
    test_type = "配对样本t检验（双尾）"
  )
)

# 练习题数据
basic_exercises <- list(
  list(
    id = 1,
    title = "置信区间计算",
    description = "某研究调查了15名学生的每日学习时间（小时），得到样本均值为3.5小时，样本标准差为0.8小时。请计算总体均值的95%置信区间。",
    data = list(n = 15, mean = 3.5, sd = 0.8, conf_level = 0.95),
    solution = "使用t分布计算置信区间：3.5 ± 2.145 × (0.8/√15) = 3.5 ± 0.443 = [3.057, 3.943]小时",
    hints = "使用小样本t分布公式：x̄ ± t(α/2, n-1) × (s/√n)"
  ),
  list(
    id = 2,
    title = "假设检验设置",
    description = "研究者想检验某学校学生的平均身高是否低于全国平均水平170厘米。请设置适当的原假设和备择假设。",
    solution = "原假设H₀: μ = 170（平均身高等于全国平均水平）\n备择假设H₁: μ < 170（平均身高低于全国平均水平）\n这是一个左尾检验。",
    hints = "考虑研究目的是检验身高是否'低于'某个值"
  ),
  list(
    id = 3,
    title = "p值解释",
    description = "在一项研究中，研究者得到p值为0.03。请解释这个p值的含义，并在α=0.05的显著性水平下做出决策。",
    solution = "p值0.03表示在原假设为真的条件下，观察到当前或更极端结果的概率为0.03。由于p值(0.03) < α(0.05)，我们拒绝原假设，结果具有统计学显著性。",
    hints = "比较p值和显著性水平α，决定是否拒绝原假设"
  ),
  list(
    id = 4,
    title = "检验功效分析",
    description = "研究者想检测两组之间的差异，预期效应量为Cohen's d = 0.5，显著性水平α = 0.05，期望功效为0.8。每组需要多少样本？",
    solution = "使用pwr.t.test函数计算：每组需要约64个样本。\npwr.t.test(d = 0.5, sig.level = 0.05, power = 0.8, type = 'two.sample', alternative = 'two.sided')",
    hints = "使用pwr包中的pwr.t.test函数，注意设置type='two.sample'"
  ),
  list(
    id = 5,
    title = "t检验类型选择",
    description = "研究者测量了同一组学生在考试前和考试后的焦虑水平。应该使用哪种t检验来分析这些数据？",
    solution = "应使用配对样本t检验（paired t-test）。因为数据来自同一组受试者在两个不同时间点的测量，样本之间存在自然配对关系。",
    hints = "考虑数据收集方式：是同一组人在不同时间点，还是不同组人在同一时间点？"
  )
)

# 综合练习数据
advanced_exercises <- list(
  list(
    id = 1,
    title = "减肥计划效果分析",
    description = "一项研究调查了某减肥计划的效果。10名参与者在参加计划前后测量了体重（公斤）：\n\n前：82.5, 79.3, 88.7, 73.2, 85.6, 92.4, 77.8, 83.1, 90.5, 76.3\n后：78.9, 76.2, 85.1, 70.8, 81.9, 88.5, 75.2, 80.0, 86.7, 73.5\n\n请进行适当的假设检验，分析减肥计划是否有显著效果。包括：\n1. 设置假设\n2. 选择适当的检验方法\n3. 计算并解释p值\n4. 计算95%置信区间\n5. 计算效应量\n6. 给出结论",
    data = sample_data$weight_loss,
    solution = "1. 原假设H₀: μd = 0（减肥前后体重无差异）\n   备择假设H₁: μd < 0（减肥后体重显著降低）\n2. 使用配对样本t检验（左尾）\n3. t检验结果：t = -16.73, df = 9, p < 0.001\n4. 平均减重的95%置信区间：[3.01, 3.94]公斤\n5. Cohen's d = 5.29（大效应）\n6. 结论：减肥计划有统计学显著的效果，平均减重约3.48公斤。",
    hints = "考虑数据是前后测量，应使用配对样本t检验；注意假设方向应为左尾（减肥后体重降低）"
  ),
  list(
    id = 2,
    title = "两种药物效果比较",
    description = "研究比较了两种降血压药物的效果。研究者随机将患者分为两组，分别使用药物A和药物B，记录了血压下降的幅度（mmHg）：\n\n药物A：12, 15, 10, 18, 14, 16, 11, 13, 17, 15, 14, 16, 13, 15\n药物B：14, 18, 16, 21, 17, 19, 15, 16, 20, 18, 17, 19, 16, 18\n\n请分析两种药物的效果是否有显著差异，并判断哪种药物效果更好。",
    data = sample_data$treatment_effect,
    solution = "1. 原假设H₀: μA = μB（两种药物效果相同）\n   备择假设H₁: μA ≠ μB（两种药物效果不同）\n2. 使用独立样本t检验（双尾）\n3. t检验结果：t = -4.57, df = 26, p < 0.001\n4. 平均差异的95%置信区间：[-4.18, -1.68]mmHg\n5. Cohen's d = 1.73（大效应）\n6. 结论：药物B的降血压效果显著优于药物A，平均多降低约2.93mmHg。",
    hints = "使用独立样本t检验比较两组；注意观察描述性统计以确定哪种药物效果更好"
  ),
  list(
    id = 3,
    title = "教学干预效果评估",
    description = "一项研究旨在评估新教学方法对学生成绩的影响。研究者测量了12名学生在干预前后的考试成绩：\n\n干预前：68, 72, 65, 82, 75, 70, 64, 83, 76, 79, 71, 74\n干预后：73, 76, 70, 85, 79, 75, 69, 88, 81, 84, 74, 79\n\n请分析新教学方法是否有显著提高学生成绩，并计算需要多少名学生才能达到90%的检验功效（假设当前观察到的效应量）。",
    data = sample_data$student_scores,
    solution = "1. 原假设H₀: μd = 0（干预前后成绩无差异）\n   备择假设H₁: μd > 0（干预后成绩显著提高）\n2. 使用配对样本t检验（右尾）\n3. t检验结果：t = 7.75, df = 11, p < 0.001\n4. 平均提高的95%置信区间：[3.46, 6.04]分\n5. Cohen's d = 2.24（大效应）\n6. 功效分析：要达到90%的检验功效，需要7名学生\n7. 结论：新教学方法显著提高了学生成绩，平均提高约4.75分。",
    hints = "使用配对样本t检验；功效分析使用pwr.t.test函数，注意使用观察到的效应量和type='paired'"
  )
)

# 辅助函数

# 格式化数值，保留指定位数
format_number <- function(x, digits = 3) {
  return(format(round(x, digits), nsmall = digits))
}

# 计算效应量 Cohen's d
calculate_cohens_d <- function(group1, group2, paired = FALSE) {
  if (paired) {
    diff <- group1 - group2
    d <- mean(diff) / sd(diff)
  } else {
    n1 <- length(group1)
    n2 <- length(group2)
    s1 <- sd(group1)
    s2 <- sd(group2)
    
    # 计算合并方差
    pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    d <- abs(mean(group1) - mean(group2)) / pooled_sd
  }
  return(d)
}

# 解释效应量大小
interpret_effect_size <- function(d) {
  if (is.na(d)) return("无法计算")
  
  if (abs(d) < 0.2) {
    return("极小效应")
  } else if (abs(d) < 0.5) {
    return("小效应")
  } else if (abs(d) < 0.8) {
    return("中等效应")
  } else {
    return("大效应")
  }
}

# 生成随机样本数据
generate_random_sample <- function(mean = 50, sd = 10, n = 30) {
  return(rnorm(n, mean, sd))
}

# 设置语言 (中文)
Sys.setlocale("LC_ALL", "zh_CN.UTF-8") 