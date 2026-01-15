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
