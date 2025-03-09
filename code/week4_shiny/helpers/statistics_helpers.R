# 统计辅助函数
# 包含用于统计计算的辅助函数

# 计算置信区间
calculate_confidence_interval <- function(data, conf_level = 0.95, is_known_sigma = FALSE, population_sigma = NULL) {
  n <- length(data)
  sample_mean <- mean(data)
  alpha <- 1 - conf_level
  
  if (is_known_sigma || n >= 30) {
    # 使用Z分布（已知总体标准差或大样本）
    if (is_known_sigma) {
      sigma <- population_sigma
    } else {
      sigma <- sd(data)  # 用样本标准差近似总体标准差
    }
    
    z_critical <- qnorm(1 - alpha/2)
    margin_of_error <- z_critical * sigma / sqrt(n)
    
    distribution <- "Z分布"
    critical_value <- z_critical
  } else {
    # 使用t分布（未知总体标准差且小样本）
    sigma <- sd(data)
    t_critical <- qt(1 - alpha/2, df = n - 1)
    margin_of_error <- t_critical * sigma / sqrt(n)
    
    distribution <- "t分布"
    critical_value <- t_critical
  }
  
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  
  return(list(
    sample_mean = sample_mean,
    sample_sd = sd(data),
    sample_size = n,
    confidence_level = conf_level,
    critical_value = critical_value,
    distribution = distribution,
    standard_error = sigma / sqrt(n),
    margin_of_error = margin_of_error,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  ))
}

# 运行单样本t检验
run_one_sample_t_test <- function(data, mu = 0, conf_level = 0.95, alternative = "two.sided") {
  # 计算基本统计量
  n <- length(data)
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  sample_se <- sample_sd / sqrt(n)
  
  # 运行t检验
  t_test_result <- t.test(
    data,
    mu = mu,
    conf.level = conf_level,
    alternative = alternative
  )
  
  # 计算效应量 (Cohen's d)
  cohens_d <- abs(sample_mean - mu) / sample_sd
  effect_size_interpretation <- interpret_effect_size(cohens_d)
  
  # 构建结果列表
  result <- list(
    test_type = "单样本t检验",
    sample_size = n,
    sample_mean = sample_mean,
    sample_sd = sample_sd,
    standard_error = sample_se,
    null_value = mu,
    alternative = alternative,
    t_statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    conf_level = conf_level,
    conf_int = t_test_result$conf.int,
    effect_size = cohens_d,
    effect_size_interpretation = effect_size_interpretation
  )
  
  return(result)
}

# 运行独立样本t检验
run_two_sample_t_test <- function(group1, group2, conf_level = 0.95, alternative = "two.sided", var_equal = FALSE) {
  # 计算基本统计量
  n1 <- length(group1)
  n2 <- length(group2)
  mean1 <- mean(group1)
  mean2 <- mean(group2)
  sd1 <- sd(group1)
  sd2 <- sd(group2)
  
  # 运行t检验
  t_test_result <- t.test(
    group1,
    group2,
    paired = FALSE,
    var.equal = var_equal,
    conf.level = conf_level,
    alternative = alternative
  )
  
  # 计算效应量 (Cohen's d)
  cohens_d <- calculate_cohens_d(group1, group2, paired = FALSE)
  effect_size_interpretation <- interpret_effect_size(cohens_d)
  
  # 构建结果列表
  result <- list(
    test_type = "独立样本t检验",
    equal_variance = var_equal,
    group1_size = n1,
    group2_size = n2,
    group1_mean = mean1,
    group2_mean = mean2,
    group1_sd = sd1,
    group2_sd = sd2,
    mean_difference = mean1 - mean2,
    alternative = alternative,
    t_statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    conf_level = conf_level,
    conf_int = t_test_result$conf.int,
    effect_size = cohens_d,
    effect_size_interpretation = effect_size_interpretation
  )
  
  return(result)
}

# 运行配对样本t检验
run_paired_t_test <- function(group1, group2, conf_level = 0.95, alternative = "two.sided") {
  # 计算基本统计量
  diff <- group1 - group2
  n <- length(diff)
  mean_diff <- mean(diff)
  sd_diff <- sd(diff)
  
  # 运行t检验
  t_test_result <- t.test(
    group1,
    group2,
    paired = TRUE,
    conf.level = conf_level,
    alternative = alternative
  )
  
  # 计算效应量 (Cohen's d)
  cohens_d <- calculate_cohens_d(group1, group2, paired = TRUE)
  effect_size_interpretation <- interpret_effect_size(cohens_d)
  
  # 构建结果列表
  result <- list(
    test_type = "配对样本t检验",
    sample_size = n,
    group1_mean = mean(group1),
    group2_mean = mean(group2),
    group1_sd = sd(group1),
    group2_sd = sd(group2),
    mean_difference = mean_diff,
    sd_difference = sd_diff,
    alternative = alternative,
    t_statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    conf_level = conf_level,
    conf_int = t_test_result$conf.int,
    effect_size = cohens_d,
    effect_size_interpretation = effect_size_interpretation
  )
  
  return(result)
}

# 格式化t检验结果为可读文本
format_t_test_result <- function(result) {
  # 初始化文本结果
  test_result_text <- ""
  
  # 添加检验类型
  test_result_text <- paste0(test_result_text, "## 检验类型\n", result$test_type)
  
  # 添加描述性统计
  test_result_text <- paste0(test_result_text, "\n\n## 描述性统计\n")
  
  if (result$test_type == "单样本t检验") {
    test_result_text <- paste0(
      test_result_text,
      "样本量: ", result$sample_size, "\n",
      "样本均值: ", format_number(result$sample_mean), "\n",
      "样本标准差: ", format_number(result$sample_sd), "\n",
      "标准误差: ", format_number(result$standard_error), "\n"
    )
  } else if (result$test_type == "独立样本t检验") {
    test_result_text <- paste0(
      test_result_text,
      "组1样本量: ", result$group1_size, "\n",
      "组1均值: ", format_number(result$group1_mean), "\n",
      "组1标准差: ", format_number(result$group1_sd), "\n\n",
      "组2样本量: ", result$group2_size, "\n",
      "组2均值: ", format_number(result$group2_mean), "\n",
      "组2标准差: ", format_number(result$group2_sd), "\n\n",
      "均值差异: ", format_number(result$mean_difference), "\n",
      "等方差假设: ", ifelse(result$equal_variance, "是", "否"), "\n"
    )
  } else if (result$test_type == "配对样本t检验") {
    test_result_text <- paste0(
      test_result_text,
      "样本量: ", result$sample_size, "\n",
      "组1均值: ", format_number(result$group1_mean), "\n",
      "组2均值: ", format_number(result$group2_mean), "\n",
      "均值差异: ", format_number(result$mean_difference), "\n",
      "差异标准差: ", format_number(result$sd_difference), "\n"
    )
  }
  
  # 添加假设
  test_result_text <- paste0(test_result_text, "\n## 假设\n")
  
  if (result$test_type == "单样本t检验") {
    null_value <- result$null_value
    if (result$alternative == "two.sided") {
      test_result_text <- paste0(
        test_result_text,
        "原假设 H₀: μ = ", null_value, "\n",
        "备择假设 H₁: μ ≠ ", null_value, "\n"
      )
    } else if (result$alternative == "greater") {
      test_result_text <- paste0(
        test_result_text,
        "原假设 H₀: μ ≤ ", null_value, "\n",
        "备择假设 H₁: μ > ", null_value, "\n"
      )
    } else if (result$alternative == "less") {
      test_result_text <- paste0(
        test_result_text,
        "原假设 H₀: μ ≥ ", null_value, "\n",
        "备择假设 H₁: μ < ", null_value, "\n"
      )
    }
  } else if (result$test_type == "独立样本t检验" || result$test_type == "配对样本t检验") {
    if (result$alternative == "two.sided") {
      test_result_text <- paste0(
        test_result_text,
        "原假设 H₀: μ₁ - μ₂ = 0\n",
        "备择假设 H₁: μ₁ - μ₂ ≠ 0\n"
      )
    } else if (result$alternative == "greater") {
      test_result_text <- paste0(
        test_result_text,
        "原假设 H₀: μ₁ - μ₂ ≤ 0\n",
        "备择假设 H₁: μ₁ - μ₂ > 0\n"
      )
    } else if (result$alternative == "less") {
      test_result_text <- paste0(
        test_result_text,
        "原假设 H₀: μ₁ - μ₂ ≥ 0\n",
        "备择假设 H₁: μ₁ - μ₂ < 0\n"
      )
    }
  }
  
  # 添加检验结果
  test_result_text <- paste0(
    test_result_text,
    "\n## 检验结果\n",
    "t统计量: ", format_number(result$t_statistic), "\n",
    "自由度: ", format_number(result$df), "\n",
    "p值: ", format_number(result$p_value), "\n",
    result$conf_level * 100, "%置信区间: [", 
    format_number(result$conf_int[1]), ", ", 
    format_number(result$conf_int[2]), "]\n"
  )
  
  # 添加效应量
  test_result_text <- paste0(
    test_result_text,
    "\n## 效应量\n",
    "Cohen's d: ", format_number(result$effect_size), " (", result$effect_size_interpretation, ")\n"
  )
  
  # 添加结论
  test_result_text <- paste0(test_result_text, "\n## 结论\n")
  if (result$p_value <= 0.05) {
    test_result_text <- paste0(
      test_result_text,
      "在α = 0.05的显著性水平下，我们拒绝原假设。\n"
    )
    
    if (result$test_type == "单样本t检验") {
      if (result$alternative == "two.sided") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明总体均值与", result$null_value, "有显著差异。"
        )
      } else if (result$alternative == "greater") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明总体均值显著大于", result$null_value, "。"
        )
      } else if (result$alternative == "less") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明总体均值显著小于", result$null_value, "。"
        )
      }
    } else if (result$test_type == "独立样本t检验") {
      if (result$alternative == "two.sided") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明两组均值有显著差异。"
        )
      } else if (result$alternative == "greater") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明组1的均值显著大于组2的均值。"
        )
      } else if (result$alternative == "less") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明组1的均值显著小于组2的均值。"
        )
      }
    } else if (result$test_type == "配对样本t检验") {
      if (result$alternative == "two.sided") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明配对前后的均值有显著差异。"
        )
      } else if (result$alternative == "greater") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明处理后的均值显著高于处理前。"
        )
      } else if (result$alternative == "less") {
        test_result_text <- paste0(
          test_result_text,
          "数据提供了足够的证据表明处理后的均值显著低于处理前。"
        )
      }
    }
  } else {
    test_result_text <- paste0(
      test_result_text,
      "在α = 0.05的显著性水平下，我们不能拒绝原假设。\n"
    )
    
    if (result$test_type == "单样本t检验") {
      if (result$alternative == "two.sided") {
        test_result_text <- paste0(
          test_result_text,
          "数据没有提供足够的证据表明总体均值与", result$null_value, "有显著差异。"
        )
      } else if (result$alternative == "greater") {
        test_result_text <- paste0(
          test_result_text,
          "数据没有提供足够的证据表明总体均值显著大于", result$null_value, "。"
        )
      } else if (result$alternative == "less") {
        test_result_text <- paste0(
          test_result_text,
          "数据没有提供足够的证据表明总体均值显著小于", result$null_value, "。"
        )
      }
    } else if (result$test_type == "独立样本t检验" || result$test_type == "配对样本t检验") {
      if (result$alternative == "two.sided") {
        test_result_text <- paste0(
          test_result_text,
          "数据没有提供足够的证据表明两组均值有显著差异。"
        )
      } else if (result$alternative == "greater") {
        test_result_text <- paste0(
          test_result_text,
          "数据没有提供足够的证据表明组1的均值显著大于组2的均值。"
        )
      } else if (result$alternative == "less") {
        test_result_text <- paste0(
          test_result_text,
          "数据没有提供足够的证据表明组1的均值显著小于组2的均值。"
        )
      }
    }
  }
  
  return(test_result_text)
}

# 计算功效分析
calculate_power_analysis <- function(d, sig_level, power, type, alternative) {
  result <- NULL
  
  tryCatch({
    result <- pwr.t.test(
      d = d,
      sig.level = sig_level,
      power = power,
      type = type,
      alternative = alternative
    )
  }, error = function(e) {
    # 处理错误情况
    return(NULL)
  })
  
  return(result)
}

# 根据参数计算需要的样本量
calculate_sample_size <- function(d, sig_level, power, type, alternative) {
  analysis_result <- calculate_power_analysis(d, sig_level, power, type, alternative)
  
  if (is.null(analysis_result)) {
    return(NA)
  }
  
  if (type == "paired" || type == "one.sample") {
    return(ceiling(analysis_result$n))
  } else if (type == "two.sample") {
    return(ceiling(analysis_result$n * 2))  # 总样本量 = 每组样本量 * 2
  }
  
  return(NA)
}

# 计算给定参数下的检验功效
calculate_power <- function(d, sig_level, n, type, alternative) {
  n_per_group <- if (type == "two.sample") n / 2 else n
  
  result <- NULL
  
  tryCatch({
    result <- pwr.t.test(
      d = d,
      sig.level = sig_level,
      n = n_per_group,
      type = type,
      alternative = alternative
    )
  }, error = function(e) {
    # 处理错误情况
    return(NULL)
  })
  
  if (is.null(result)) {
    return(NA)
  }
  
  return(result$power)
} 