# Shiny可视化应用：Arc/Chord/Sankey/HEB图展示
![Shiny](https://img.shields.io/badge/Shiny-1.7.4-348FE2.svg)
![R](https://img.shields.io/badge/R-≥4.0.0-276DC3.svg)
![License](https://img.shields.io/badge/License-MIT-blue.svg)

## 项目简介
这是一个基于**模块化开发思想**构建的R Shiny可视化应用，支持Arc图、Chord图、Sankey图、HEB图（热力图变体）的动态展示。项目采用分模块设计，将UI、Server逻辑、绘图函数、工具函数解耦，便于维护和扩展，适合数据可视化初学者学习Shiny模块化开发，也可直接适配自有数据快速落地可视化需求。

## ✨ 功能特点
- **模块化架构**：UI/Server/绘图函数拆分，代码结构清晰，易维护、易扩展；
- **多类型可视化**：支持Arc/Chord/Sankey/HEB四种常见关联数据可视化类型；
- **交互性强**：可动态选择图表类型、调整数据维度、一键刷新数据；
- **样式定制化**：内置自定义CSS样式，支持响应式布局（适配电脑/手机）；
- **数据兜底**：无自定义数据时自动生成随机测试数据，开箱即用；
- **鲁棒性高**：包含工具函数兜底，避免空值/异常数据导致应用崩溃。

## 📋 环境准备
### 1. 基础软件安装
- **R语言**：下载地址 → [CRAN官方](https://cran.r-project.org/)（推荐版本 ≥ 4.0.0）；
- **RStudio**（可选但推荐）：下载地址 → [Posit官网](https://posit.co/download/rstudio-desktop/)（免费版即可）。

### 2. 依赖包安装
打开R/RStudio控制台，执行以下命令安装项目所需依赖包：
```r
# 核心Shiny包
install.packages("shiny")
# 可视化&数据处理依赖包
install.packages(c(
  "ggplot2",    # 基础可视化
  "networkD3",  # Sankey图
  "circlize",   # Arc/Chord图
  "dplyr",      # 数据处理
  "tidyr",      # 数据重塑
  "reshape2"    # 矩阵转长格式
))
📖 使用说明
1. 界面操作指南
组件	功能说明
图表类型下拉框	选择要展示的可视化类型（Arc/Chord/Sankey/HEB）
数据维度输入框	调整随机数据的行列数（3-10，默认 5）
刷新数据按钮	重新生成随机数据并更新图表
主面板图表区	展示所选类型的可视化结果（高度 600px）
2. 替换为自有数据
步骤 1：准备数据文件
将你的数据保存为CSV格式（矩阵形式，行 / 列有名称），示例格式：
Col1	Col2	Col3
Row1	1.2	3.4	5.6
Row2	2.3	4.5	6.7
Row3	3.4	5.6	7.8
步骤 2：替换数据文件
将自有数据文件命名为example_matrix.csv，放入项目data/目录下，覆盖原有文件（若无则新建）。
步骤 3：重启应用
替换数据后，停止当前 Shiny 应用并重新运行，应用会自动读取你的自定义数据。
🛠️ 自定义扩展
1. 新增图表类型
步骤 1：在R/04_plot_functions.R中添加绘图函数
r
运行
# 示例：新增散点图函数
plot_scatter <- function(data) {
  df <- as.data.frame(data)
  df$row <- rownames(df)
  df_long <- tidyr::pivot_longer(df, -row, names_to = "col", values_to = "value")
  
  ggplot2::ggplot(df_long, aes(x = col, y = value, color = row)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_minimal()
}
步骤 2：在R/02_ui_modules.R中添加下拉选项
修改selectInput的choices参数：
r
运行
selectInput(ns("plot_type"), "选择图表类型：",
            choices = c("Arc图", "Chord图", "Sankey图", "HEB图", "散点图")) # 新增散点图
步骤 3：在R/03_server_modules.R中绑定逻辑
修改switch语句，新增散点图分支：
r
运行
output$plot <- switch(
  plot_type,
  "Arc图" = renderPlot(plot_arc(data)),
  "Chord图" = renderPlot(plot_chord(data)),
  "Sankey图" = renderNetworkD3(plot_sankey(data)),
  "HEB图" = renderPlot(plot_heb(data)),
  "散点图" = renderPlot(plot_scatter(data)) # 新增散点图逻辑
)
2. 修改界面样式
编辑www/custom.css文件即可自定义样式：
调整颜色：修改background-color/color的值；
调整布局：修改width/margin/padding；
响应式适配：修改@media (max-width: 768px)中的样式；
修改后重启应用生效。
❓ 常见问题与解决方案
Q1：运行应用时提示 “找不到函数 / 包”？
原因：依赖包未安装或安装不完整；
解决方案：重新执行「环境准备」中的依赖包安装命令，确保无报错。
Q2：CSS 样式修改后不生效？
原因：浏览器缓存或未重启应用；
解决方案：
停止应用并重新运行；
浏览器按Ctrl+F5（Windows）/Cmd+Shift+R（Mac）强制刷新缓存。
Q3：提交 Git 时提示 “Untracked files present”？
原因：未将文件加入 Git 暂存区；
解决方案：
bash
运行
git add app.R R/ www/ data/ # 添加文件
git commit -m'新增/修改：xxx' # 提交
git push origin main # 推送到远程仓库
Q4：Sankey 图显示异常？
原因：数据格式不符合 networkD3 要求；
解决方案：确保数据为矩阵形式，行 / 列名称无重复，数值为数值型。
📄 许可证
本项目采用 MIT 许可证开源，你可自由使用、修改、分发本项目代码（需保留许可证声明）。
📬 联系作者
作者：[你的名字]
邮箱：[你的邮箱]
项目地址：https://github.com/你的 GitHub 用户名 / WJ-Liu1.github.io
