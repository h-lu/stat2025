#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| message: false
# 首先加载必要的包
library(tidyverse)
library(palmerpenguins)

# 创建新列：计算每只企鹅的体重与体长的比值
penguins <- penguins %>%
  mutate(weight_length_ratio = body_mass_g / bill_length_mm)

# 修改现有列：将体重从克转换为千克
penguins <- penguins %>%
  mutate(body_mass_kg = body_mass_g / 1000)
```
#
#
#
#
#
#
#
# 按企鹅体重降序排序
penguins %>%
  arrange(desc(body_mass_g))

# 按企鹅种类升序、体重降序排序
penguins %>%
  arrange(species, desc(body_mass_g))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 将列名 bill_length_mm 重命名为 beak_length
penguins %>%
  rename(beak_length = bill_length_mm)

# 同时重命名多列
penguins %>%
  rename(beak_length = bill_length_mm, beak_depth = bill_depth_mm)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 去除 penguins 中的重复行
penguins %>%
  distinct()

# 基于指定列去除重复行 (例如，只考虑企鹅种类和性别)
penguins %>%
  distinct(species, sex)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 计算所有企鹅的平均体重和体重标准差
penguins %>%
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE)
  )

# 分组计算：按企鹅种类计算平均体重
penguins %>%
  group_by(species) %>%
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    n_penguins = n() # 统计每种类型企鹅的数量
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
# 按企鹅种类分组
penguins %>%
  group_by(species)

# 按企鹅种类和性别分组
penguins %>%
  group_by(species, sex)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 链式操作示例：
# 1. 选择种类、体重和嘴峰长度列
# 2. 筛选体重大于 4000g 的企鹅
# 3. 按企鹅种类分组
# 4. 汇总计算每种类型企鹅的平均体重和数量
penguins %>%
  select(species, body_mass_g, bill_length_mm) %>%
  filter(body_mass_g > 4000) %>%
  group_by(species) %>%
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    n_penguins = n()
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
# 示例数据：宽格式的学生成绩数据
wide_data <- data.frame(
  姓名 = c("小明", "小红"),
  语文成绩 = c(80, 90),
  数学成绩 = c(85, 92),
  英语成绩 = c(78, 88)
)

# 使用 pivot_longer() 将宽格式数据转换为长格式
wide_data %>%
  pivot_longer(
    cols = c(语文成绩, 数学成绩, 英语成绩), # 指定要转换的列
    names_to = "科目",        # 转换后的列名，存储原来的列名 (语文成绩, 数学成绩, 英语成绩)
    values_to = "成绩"       # 转换后的列名，存储原来的列值 (80, 90, 85, 92, 78, 88)
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
# 示例数据：长格式的学生成绩数据 (上例转换后的 long_data)

# 使用 pivot_wider() 将长格式数据转换为宽格式
long_data %>%
  pivot_wider(
    names_from = "科目",      # 指定列名来源，科目列的取值 (语文, 数学, 英语) 将作为新的列名
    values_from = "成绩"     # 指定列值来源，成绩列的值将作为新列的值
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
# 示例数据：包含 "年份-月份-日期" 格式日期的列
date_data <- data.frame(
  日期 = c("2023-10-26", "2023-10-27", "2023-10-28"),
  value = c(100, 120, 110)
)

# 使用 separate() 将 "日期" 列拆分成 "年份", "月份", "日期" 三列
separated_data <-date_data %>%
  separate(
    col = 日期,          # 指定要拆分的列
    into = c("年份", "月份", "日期"), # 拆分后的新列名
    sep = "-"           # 分隔符为 "-"
  )

separated_data
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# 示例数据：包含 "年份", "月份", "日期" 列的数据 (上例拆分后的 separated_data)

# 使用 unite() 将 "年份", "月份", "日期" 三列合并成 "完整日期" 列
separated_data %>%
  unite(
    col = 完整日期,       # 合并后的新列名
    年份, 月份, 日期,      # 要合并的列
    sep = "-"           # 分隔符为 "-"
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
    ggplot(data = penguins, aes(x = body_mass_g)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black")
#
#
#
#
#
    ggplot(data = penguins, aes(x = bill_length_mm, y = body_mass_g)) +
      geom_point(color = "steelblue", alpha = 0.5)
#
#
#
#
#
    ggplot(data = penguins, aes(x = species, y = body_mass_g)) +
      geom_boxplot(fill = "lightcoral", color = "black")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
