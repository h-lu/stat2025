# 绘图辅助函数
# 包含用于创建常见图表的函数

# 定义一个现代化主题，用于所有图表
theme_modern <- function() {
  theme_minimal() +
  theme(
    # 使用更现代的字体
    text = element_text(family = "roboto", color = app_colors$text_dark),
    plot.title = element_text(family = "notosans", face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(family = "notosans", hjust = 0.5, color = app_colors$chart_gray, size = 12),
    
    # 坐标轴样式
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    
    # 图例样式
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    
    # 面板样式
    panel.grid.major = element_line(color = "#eeeeee"),
    panel.grid.minor = element_line(color = "#f5f5f5"),
    
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
}

# 绘制置信区间可视化图
plot_confidence_interval <- function(data, ci_result) {
  # 生成数据分布图
  p <- ggplot() +
    # 添加数据分布的密度曲线
    geom_density(data = data.frame(x = data), aes(x = x), fill = app_colors$chart_primary, alpha = 0.3) +
    # 添加样本均值线
    geom_vline(xintercept = ci_result$sample_mean, color = app_colors$chart_danger, linetype = "dashed", linewidth = 1) +
    # 添加置信区间
    geom_segment(aes(x = ci_result$lower_bound, y = 0.05, xend = ci_result$upper_bound, yend = 0.05),
                 color = app_colors$chart_secondary, linewidth = 2, arrow = arrow(ends = "both", length = unit(0.1, "inches"))) +
    # 标注样本均值
    annotate("text", x = ci_result$sample_mean, y = max(density(data)$y) * 0.7,
             label = paste0("样本均值 = ", round(ci_result$sample_mean, 2)),
             color = app_colors$chart_danger, size = 4, hjust = -0.1, family = "notosans") +
    # 标注置信区间
    annotate("text", x = mean(c(ci_result$lower_bound, ci_result$upper_bound)), y = 0.07,
             label = paste0(ci_result$confidence_level * 100, "% 置信区间: [",
                           round(ci_result$lower_bound, 2), ", ", round(ci_result$upper_bound, 2), "]"),
             color = app_colors$chart_secondary, size = 4, family = "notosans") +
    # 设置标题和轴标签
    labs(title = paste0(ci_result$confidence_level * 100, "% 置信区间可视化"),
         subtitle = paste0("样本大小: ", ci_result$sample_size, ", 样本标准差: ", round(ci_result$sample_sd, 2)),
         x = "值", y = "密度") +
    # 应用现代主题
    theme_modern()
  
  return(p)
}

# 绘制假设检验错误类型可视化图
plot_hypothesis_testing_errors <- function(mu0 = 100, mu1 = 110, sigma = 15, alpha = 0.05) {
  # 计算临界值
  critical_value <- mu0 + qnorm(1 - alpha) * sigma
  
  # 创建数据点
  x_range <- seq(mu0 - 4 * sigma, mu1 + 4 * sigma, length.out = 1000)
  h0_density <- dnorm(x_range, mean = mu0, sd = sigma)
  h1_density <- dnorm(x_range, mean = mu1, sd = sigma)
  
  # 计算错误区域
  type1_error_x <- x_range[x_range >= critical_value & x_range <= mu0 + 4 * sigma]
  type1_error_y <- dnorm(type1_error_x, mean = mu0, sd = sigma)
  
  type2_error_x <- x_range[x_range <= critical_value & x_range >= mu1 - 4 * sigma]
  type2_error_y <- dnorm(type2_error_x, mean = mu1, sd = sigma)
  
  # 计算两种错误的概率
  type1_error_prob <- 1 - pnorm(critical_value, mean = mu0, sd = sigma)
  type2_error_prob <- pnorm(critical_value, mean = mu1, sd = sigma)
  power <- 1 - type2_error_prob
  
  # 创建图形
  p <- ggplot() +
    # H0分布
    geom_line(data = data.frame(x = x_range, y = h0_density),
              aes(x = x, y = y, color = "H₀ 分布 (μ = 100)"), linewidth = 1) +
    # H1分布
    geom_line(data = data.frame(x = x_range, y = h1_density),
              aes(x = x, y = y, color = "H₁ 分布 (μ = 110)"), linewidth = 1) +
    # Type I 错误区域
    geom_area(data = data.frame(x = type1_error_x, y = type1_error_y),
              aes(x = x, y = y, fill = "Type I 错误 (α)"), alpha = 0.5) +
    # Type II 错误区域
    geom_area(data = data.frame(x = type2_error_x, y = type2_error_y),
              aes(x = x, y = y, fill = "Type II 错误 (β)"), alpha = 0.5) +
    # 功效区域
    geom_area(data = data.frame(
      x = x_range[x_range >= critical_value & x_range >= mu1 - 4 * sigma],
      y = dnorm(x_range[x_range >= critical_value & x_range >= mu1 - 4 * sigma], mean = mu1, sd = sigma)
    ), aes(x = x, y = y, fill = "检验功效 (1-β)"), alpha = 0.5) +
    # 临界值线
    geom_vline(xintercept = critical_value, linetype = "dashed", color = app_colors$chart_gray, linewidth = 1.2) +
    # 添加文本标签
    annotate("text", x = critical_value + 5, y = max(h0_density, h1_density) * 0.9,
             label = paste0("临界值 = ", round(critical_value, 1)), size = 4, family = "notosans") +
    # 设置颜色和填充
    scale_color_manual(name = "分布",
                      values = c("H₀ 分布 (μ = 100)" = app_colors$chart_primary,
                                "H₁ 分布 (μ = 110)" = app_colors$chart_secondary)) +
    scale_fill_manual(name = "错误类型",
                     values = c("Type I 错误 (α)" = app_colors$chart_danger,
                               "Type II 错误 (β)" = app_colors$chart_warning,
                               "检验功效 (1-β)" = app_colors$chart_success)) +
    # 设置标题和轴标签
    labs(title = "假设检验中的两类错误和检验功效",
         subtitle = paste0("Type I 错误 (α) = ", round(type1_error_prob, 3),
                          ", Type II 错误 (β) = ", round(type2_error_prob, 3),
                          ", 检验功效 (1-β) = ", round(power, 3)),
         x = "观察值", y = "密度") +
    # 应用现代主题
    theme_modern() +
    theme(
      legend.box = "vertical",
      legend.margin = margin(t = 10)
    )
  
  return(p)
}

# 绘制功效分析图
plot_power_analysis <- function(effect_size_range = seq(0.1, 1, by = 0.1),
                               sig_level = 0.05,
                               sample_sizes = c(10, 20, 30, 50, 100),
                               type = "two.sample",
                               alternative = "two.sided") {
  
  # 创建空数据框
  power_data <- expand.grid(
    effect_size = effect_size_range,
    sample_size = sample_sizes
  )
  
  # 计算功效
  power_data$power <- mapply(
    function(d, n) {
      n_per_group <- if (type == "two.sample") n / 2 else n
      power_result <- pwr.t.test(
        d = d,
        n = n_per_group,
        sig.level = sig_level,
        type = type,
        alternative = alternative
      )
      return(power_result$power)
    },
    power_data$effect_size,
    power_data$sample_size
  )
  
  # 将样本量转换为因子，以便在图中使用不同的线条
  power_data$sample_size <- factor(power_data$sample_size,
                                  levels = sample_sizes,
                                  labels = paste0("n = ", sample_sizes))
  
  # 创建图形
  p <- ggplot(power_data, aes(x = effect_size, y = power, color = sample_size, group = sample_size)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    # 添加0.8功效水平线
    geom_hline(yintercept = 0.8, linetype = "dashed", color = app_colors$chart_gray, linewidth = 0.8) +
    annotate("text", x = min(effect_size_range), y = 0.82, label = "功效 = 0.8", 
             hjust = 0, family = "notosans", color = app_colors$chart_gray) +
    # 设置颜色
    scale_color_viridis_d(name = "样本量", option = "plasma", begin = 0.2, end = 0.8) +
    # 设置坐标范围
    coord_cartesian(ylim = c(0, 1)) +
    # 设置标题和轴标签
    labs(title = "不同样本量和效应量下的检验功效",
         subtitle = paste0("检验类型: ", 
                          ifelse(type == "one.sample", "单样本t检验",
                                ifelse(type == "two.sample", "独立样本t检验", "配对样本t检验")),
                          ", 显著性水平: ", sig_level),
         x = "效应量 (Cohen's d)",
         y = "检验功效 (1-β)") +
    # 应用现代主题
    theme_modern()
  
  return(p)
}

# 绘制样本量计算图
plot_sample_size_calculation <- function(effect_size_range = seq(0.1, 1, by = 0.1),
                                        sig_level = 0.05,
                                        power_levels = c(0.8, 0.9, 0.95),
                                        type = "two.sample",
                                        alternative = "two.sided") {
  
  # 创建空数据框
  sample_size_data <- expand.grid(
    effect_size = effect_size_range,
    power = power_levels
  )
  
  # 计算样本量
  sample_size_data$sample_size <- mapply(
    function(d, p) {
      result <- pwr.t.test(
        d = d,
        power = p,
        sig.level = sig_level,
        type = type,
        alternative = alternative
      )
      
      if (type == "two.sample") {
        return(ceiling(result$n * 2))  # 对于独立样本，返回总样本量
      } else {
        return(ceiling(result$n))      # 对于单样本和配对样本，直接返回n
      }
    },
    sample_size_data$effect_size,
    sample_size_data$power
  )
  
  # 限制y轴范围
  max_sample_size <- 1000
  sample_size_data$sample_size[sample_size_data$sample_size > max_sample_size] <- max_sample_size
  
  # 将功效水平转换为因子
  sample_size_data$power <- factor(sample_size_data$power, 
                                  levels = power_levels,
                                  labels = paste0("功效 = ", power_levels))
  
  # 创建图形
  p <- ggplot(sample_size_data, aes(x = effect_size, y = sample_size, color = power, group = power)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    # 设置颜色
    scale_color_viridis_d(name = "检验功效", option = "viridis", begin = 0.3, end = 0.8) +
    # Y轴对数刻度
    scale_y_log10(breaks = c(10, 20, 50, 100, 200, 500, 1000),
                  labels = c("10", "20", "50", "100", "200", "500", "1000")) +
    # 设置标题和轴标签
    labs(title = "达到指定功效所需的样本量",
         subtitle = paste0("检验类型: ", 
                          ifelse(type == "one.sample", "单样本t检验",
                                ifelse(type == "two.sample", "独立样本t检验", "配对样本t检验")),
                          ", 显著性水平: ", sig_level),
         x = "效应量 (Cohen's d)",
         y = "样本量 (对数刻度)") +
    # 添加注释
    annotate("text", x = max(effect_size_range), y = max_sample_size,
             label = "注意：样本量上限为1000", hjust = 1, vjust = 1, size = 3.5,
             family = "notosans", color = app_colors$chart_gray, fontface = "italic") +
    # 应用现代主题
    theme_modern()
  
  return(p)
}

# 绘制t检验结果可视化图
plot_t_test <- function(data1, data2 = NULL, test_result, test_type) {
  if (test_type == "one.sample") {
    # 单样本t检验绘图
    p <- ggplot() +
      # 添加数据分布的密度曲线
      geom_density(data = data.frame(x = data1), aes(x = x), fill = app_colors$chart_primary, alpha = 0.5) +
      # 添加样本均值线
      geom_vline(xintercept = mean(data1), color = app_colors$chart_danger, linetype = "dashed", linewidth = 1.2) +
      # 添加原假设值线
      geom_vline(xintercept = test_result$null_value, color = app_colors$chart_gray, linetype = "dotted", linewidth = 1.2) +
      # 添加置信区间
      geom_segment(aes(x = test_result$conf_int[1], y = 0.05, xend = test_result$conf_int[2], yend = 0.05),
                   color = app_colors$chart_success, linewidth = 2.5, arrow = arrow(ends = "both", length = unit(0.15, "inches"))) +
      # 标注样本均值
      annotate("text", x = mean(data1), y = max(density(data1)$y) * 0.8,
               label = paste0("样本均值 = ", round(mean(data1), 2)),
               color = app_colors$chart_danger, size = 4.5, hjust = -0.1, family = "notosans") +
      # 标注原假设值
      annotate("text", x = test_result$null_value, y = max(density(data1)$y) * 0.6,
               label = paste0("原假设值 = ", test_result$null_value),
               color = app_colors$chart_gray, size = 4.5, hjust = 1.1, family = "notosans") +
      # 标注置信区间
      annotate("text", x = mean(c(test_result$conf_int[1], test_result$conf_int[2])), y = 0.07,
               label = paste0(test_result$conf_level * 100, "% 置信区间: [",
                             round(test_result$conf_int[1], 2), ", ", round(test_result$conf_int[2], 2), "]"),
               color = app_colors$chart_success, size = 4.5, family = "notosans") +
      # 设置标题和轴标签
      labs(title = "单样本t检验结果可视化",
           subtitle = paste0("t = ", round(test_result$t_statistic, 2), 
                            ", df = ", round(test_result$df, 1), 
                            ", p = ", round(test_result$p_value, 3)),
           x = "值", y = "密度") +
      # 应用现代主题
      theme_modern()
    
  } else if (test_type == "two.sample") {
    # 独立样本t检验绘图
    
    # 组合数据用于绘图
    plot_data <- data.frame(
      value = c(data1, data2),
      group = factor(c(rep("组1", length(data1)), rep("组2", length(data2))))
    )
    
    p <- ggplot(plot_data, aes(x = value, fill = group)) +
      # 添加密度曲线
      geom_density(alpha = 0.7) +
      # 添加均值线
      geom_vline(xintercept = mean(data1), color = app_colors$chart_primary, linetype = "dashed", linewidth = 1.2) +
      geom_vline(xintercept = mean(data2), color = app_colors$chart_secondary, linetype = "dashed", linewidth = 1.2) +
      # 添加置信区间
      annotate("rect", 
              xmin = test_result$conf_int[1] + mean(data2), 
              xmax = test_result$conf_int[2] + mean(data2), 
              ymin = 0, ymax = 0.03,
              alpha = 0.3, fill = app_colors$chart_info) +
      # 标注均值
      annotate("text", x = mean(data1), y = max(density(data1)$y) * 0.9,
               label = paste0("组1均值 = ", round(mean(data1), 2)),
               color = app_colors$chart_primary, size = 4.5, hjust = -0.1, family = "notosans") +
      annotate("text", x = mean(data2), y = max(density(data2)$y) * 0.9,
               label = paste0("组2均值 = ", round(mean(data2), 2)),
               color = app_colors$chart_secondary, size = 4.5, hjust = 1.1, family = "notosans") +
      # 标注均值差异
      annotate("text", x = mean(c(mean(data1), mean(data2))), y = 0.04,
               label = paste0("均值差异: ", round(mean(data1) - mean(data2), 2), 
                             ", ", test_result$conf_level * 100, "% CI: [", 
                             round(test_result$conf_int[1], 2), ", ", 
                             round(test_result$conf_int[2], 2), "]"),
               color = app_colors$chart_info, size = 4.5, family = "notosans") +
      # 设置颜色
      scale_fill_manual(values = c("组1" = app_colors$chart_primary, "组2" = app_colors$chart_secondary)) +
      # 设置标题和轴标签
      labs(title = "独立样本t检验结果可视化",
           subtitle = paste0("t = ", round(test_result$t_statistic, 2), 
                            ", df = ", round(test_result$df, 1), 
                            ", p = ", round(test_result$p_value, 3),
                            ", Cohen's d = ", round(test_result$effect_size, 2)),
           x = "值", y = "密度") +
      # 应用现代主题
      theme_modern()
    
  } else if (test_type == "paired") {
    # 配对样本t检验绘图
    
    # 计算差异
    diff <- data1 - data2
    
    # 准备绘图数据
    plot_data <- data.frame(
      value = c(data1, data2),
      group = factor(c(rep("前测", length(data1)), rep("后测", length(data2))))
    )
    
    # 创建箱线图和配对线条
    p1 <- ggplot() +
      # 箱线图
      geom_boxplot(data = plot_data, aes(x = group, y = value, fill = group), alpha = 0.7, width = 0.5) +
      # 个体连线
      geom_line(data = data.frame(
        x = rep(c(1, 2), each = length(data1)),
        y = c(rbind(data1, data2)),
        id = rep(1:length(data1), 2)
      ), aes(x = x, y = y, group = id), color = app_colors$chart_gray, alpha = 0.6) +
      # 均值点
      stat_summary(data = plot_data, aes(x = group, y = value, color = group),
                  fun = mean, geom = "point", size = 5) +
      # 设置颜色
      scale_fill_manual(values = c("前测" = app_colors$chart_primary, "后测" = app_colors$chart_secondary)) +
      scale_color_manual(values = c("前测" = app_colors$chart_primary, "后测" = app_colors$chart_secondary)) +
      # 设置标题和轴标签
      labs(title = "配对样本前后测比较",
           x = "", y = "值") +
      # 应用现代主题
      theme_modern() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 12)
      )
    
    # 创建差异的密度图
    p2 <- ggplot() +
      # 差异的密度曲线
      geom_density(data = data.frame(x = diff), aes(x = x), fill = app_colors$chart_info, alpha = 0.7) +
      # 添加零线
      geom_vline(xintercept = 0, color = app_colors$chart_gray, linetype = "dotted", linewidth = 1.2) +
      # 添加均值线
      geom_vline(xintercept = mean(diff), color = app_colors$chart_danger, linetype = "dashed", linewidth = 1.2) +
      # 添加置信区间
      geom_segment(aes(x = test_result$conf_int[1], y = 0.05, xend = test_result$conf_int[2], yend = 0.05),
                   color = app_colors$chart_success, linewidth = 2.5, arrow = arrow(ends = "both", length = unit(0.15, "inches"))) +
      # 标注均值差异
      annotate("text", x = mean(diff), y = max(density(diff)$y) * 0.8,
               label = paste0("平均差异 = ", round(mean(diff), 2)),
               color = app_colors$chart_danger, size = 4.5, hjust = -0.1, family = "notosans") +
      # 标注置信区间
      annotate("text", x = mean(c(test_result$conf_int[1], test_result$conf_int[2])), y = 0.07,
               label = paste0(test_result$conf_level * 100, "% 置信区间: [",
                             round(test_result$conf_int[1], 2), ", ", round(test_result$conf_int[2], 2), "]"),
               color = app_colors$chart_success, size = 4.5, family = "notosans") +
      # 设置标题和轴标签
      labs(title = "配对差异分布",
           subtitle = paste0("t = ", round(test_result$t_statistic, 2), 
                            ", df = ", round(test_result$df, 1), 
                            ", p = ", round(test_result$p_value, 3),
                            ", Cohen's d = ", round(test_result$effect_size, 2)),
           x = "差异 (前测 - 后测)", y = "密度") +
      # 应用现代主题
      theme_modern()
    
    # 使用patchwork将两张图组合
    library(patchwork)
    p <- p1 / p2 + plot_layout(heights = c(1, 1.5))
  }
  
  return(p)
} 