# app.R
# 安装必要包（首次运行时执行）
# install.packages(c("circlize", "viridis", "RColorBrewer", "networkD3", "ggraph", "ggtree", "webshot2", "phangorn", "shinyjs", "shinyjqui", "colourpicker", "bslib", "arcdiagram"))
# webshot2::install_phantomjs()

# 加载依赖包
library(shiny)
library(tidyverse)
library(igraph)
library(shinyjs)
library(shinyjqui)
library(colourpicker)
library(reshape2)
library(arcdiagram)
library(bslib)
library(circlize)
library(viridis)
library(RColorBrewer)
library(networkD3)
library(ggraph)
library(htmlwidgets)
library(webshot2)

# 加载所有模块
source("R/01_utils.R")
source("R/02_ui_modules.R")
source("R/03_server_modules.R")
source("R/04_plot_functions.R")

# 启动Shiny应用
shinyApp(ui = app_ui(), server = app_server)