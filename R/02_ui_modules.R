# R/02_ui_modules.R
# 标题栏UI
title_ui <- function() {
  div(class = "title", 
      "shiny交互式可视化应用", 
      actionButton("help_button", "Help", class = "help")
  )
}

# 侧边栏UI
sidebar_ui <- function() {
  sidebarPanel(
    class = "sidebar",
    width = 3,
    div(class = "sidebar-scroll",
        h3("可视化工具选择"),
        selectInput("chart_type", label = "选择图表类型:",
                    choices = c("请选择...", 
                                "Arc Diagram",
                                "Chord Diagram",
                                "Sankey Diagram", 
                                "Hierarchical Edge Bundling"), 
                    selected = "Arc Diagram"),
        
        downloadButton("downloadExample_mis", "下载随机示例CSV（矩阵结构）", class = "download-button"),
        hr(),
        verbatimTextOutput("upload_status"),
        hr(),
        
        h3("可视化设置"),
        # Arc Diagram设置
        conditionalPanel(
          condition = "input.chart_type == 'Arc Diagram'",
          div(class = "sidebar-group",
              h5(strong("数据来源")),
              radioButtons("arc_data_source", "选择数据:",
                           choices = c("上传矩阵数据" = "upload", "随机生成数据" = "random"),
                           selected = "random"),
              h5("数据输入 (CSV格式)"),
              fileInput("file_upload_arc", "上传CSV文件",
                        accept = c(".csv"), 
                        multiple = FALSE,
                        buttonLabel = "选择文件",
                        placeholder = "请上传矩阵结构的CSV文件"),
              h5(strong("随机数据生成（未上传时使用）")),
              sliderInput("n_nodes_arc", "节点数量:", min = 3, max = 15, value = 8, step = 1),
              numericInput("seed_arc", "随机种子:", value = 42, min = 1),
              actionButton("refresh_data_arc", "生成随机数据并绘图", icon = icon("refresh")),
              h5(strong("节点 (Node) 设置")),
              colourInput("pointColor", "默认节点颜色:", value = "#69b3a2"),
              selectInput("nodeShape", "节点形状:", choices = c("Circle" = 21, "Square" = 22, "Triangle" = 24, "Diamond" = 23), selected = 21),
              sliderInput("node_size_multiplier", "节点大小乘数:", min = 0.5, max = 2.0, value = 1.0, step = 0.1),
              h5(strong("弧/边 (Arc/Edge) 设置")),
              colourInput("lineColor", "弧线颜色:", value = "#4d4d4d"),
              selectInput("lineType", "弧线类型:", choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted"), selected = "solid"),
              sliderInput("arc_thickness_multiplier", "弧线粗细乘数:", min = 0.5, max = 3.0, value = 1.5, step = 0.1),
              h5(strong("标签 & 标题")),
              sliderInput("textSize", "标签文本大小 (相对缩放):", min = 2, max = 10, value = 5),
              checkboxInput("addTitle", "添加图表标题", value = FALSE),
              conditionalPanel(condition = "input.addTitle", textInput("chartTitle", "输入标题:", value = "Network Arc Diagram"))
          )
        ),
        # Chord Diagram设置
        conditionalPanel(
          condition = "input.chart_type == 'Chord Diagram'",
          div(class = "sidebar-group",
              h5(strong("数据来源")),
              radioButtons("chord_data_source", "选择数据:",
                           choices = c("上传矩阵数据" = "upload", "随机生成数据" = "random"),
                           selected = "random"),
              h5("数据输入 (CSV格式)"),
              fileInput("file_upload_chord", "上传CSV文件",
                        accept = c(".csv"), 
                        multiple = FALSE,
                        buttonLabel = "选择文件",
                        placeholder = "请上传矩阵结构的CSV文件"),
              h5(strong("随机数据生成（未上传时使用）")),
              sliderInput("n_regions_chord", "区域数量:", min = 3, max = 15, value = 10, step = 1),
              numericInput("seed_chord", "随机种子:", value = 42, min = 1),
              actionButton("refresh_data_chord", "生成随机数据并绘图", icon = icon("refresh")),
              h5(strong("颜色方案")),
              selectInput("color_scheme_chord", "颜色方案:",
                          choices = c("viridis", "magma", "inferno", "plasma", "cividis", "RColorBrewer"),
                          selected = "viridis"),
              conditionalPanel(
                condition = "input.color_scheme_chord == 'RColorBrewer'",
                selectInput("brewer_palette_chord", "颜色调色板:",
                            choices = c("Set1", "Dark2", "Pastel1", "Accent"),
                            selected = "Set1")
              ),
              h5(strong("链接与方向")),
              sliderInput("transparency_chord", "透明度:", min = 0.1, max = 0.9, value = 0.3, step = 0.1),
              checkboxInput("directional_chord", "显示方向", value = TRUE),
              sliderInput("direction_type_chord", "方向差异 (箭头/高度):", min = 0, max = 1, value = 0.5, step = 0.1),
              sliderInput("link_thickness_chord", "连接线基础粗细:", min = 0.1, max = 2, value = 1, step = 0.1),
              h5(strong("文本设置")),
              sliderInput("text_size_chord", "文本大小 (缩放因子):", min = 0.5, max = 1.5, value = 0.8, step = 0.1)
          )
        ),
        # Sankey Diagram设置
        conditionalPanel(
          condition = "input.chart_type == 'Sankey Diagram'",
          div(class = "sidebar-group",
              h5(strong("数据来源")),
              radioButtons("sankey_data_source", "选择数据:",
                           choices = c("上传矩阵数据" = "upload", "随机生成数据" = "random"),
                           selected = "random"),
              h5("数据输入 (CSV格式)"),
              fileInput("file_upload_sankey", "上传CSV文件",
                        accept = c(".csv"), 
                        multiple = FALSE,
                        buttonLabel = "选择文件",
                        placeholder = "请上传矩阵结构的CSV文件"),
              h5(strong("随机数据生成（未上传时使用）")),
              sliderInput("n_regions_sankey", "区域数量:", min = 3, max = 15, value = 10, step = 1),
              numericInput("seed_sankey", "随机种子:", value = 42, min = 1),
              actionButton("refresh_data_sankey", "生成随机数据并绘图", icon = icon("refresh")),
              hr(),
              h5(strong("Sankey Diagram 设置")),
              sliderInput("sankey_node_width", "节点宽度:", min = 5, max = 30, value = 10),
              sliderInput("sankey_text_size", "文本大小:", min = 10, max = 20, value = 12),
              textInput("sankey_unit", "单位 (显示在 Value 后):", value = "单位"),
              colourInput("sankey_color", "流默认颜色:", value = "lightgray"),
              sliderInput("sankey_iterations", "布局迭代次数:", min = 1, max = 50, value = 32),
              checkboxInput("sankey_node_shadow", "显示节点阴影:", value = TRUE)
          )
        ),
        # HEB设置
        conditionalPanel(
          condition = "input.chart_type == 'Hierarchical Edge Bundling'",
          div(class = "sidebar-group",
              selectInput("heb_data_source", "数据源", choices = c("随机数据" = "random", "上传数据" = "upload")),
              numericInput("n_regions_heb", "随机节点数", value = 50, min = 5, max = 50),
              numericInput("seed_heb", "随机种子", value = 200),
              fileInput("file_upload_heb", "上传矩阵文件（CSV）", accept = ".csv"),
              actionButton("refresh_data_heb", "刷新随机数据"),
              br(), br(),
              sliderInput("heb_bundling", "边捆绑张力", min = 0, max = 2, value = 0.8, step = 0.1),
              sliderInput("heb_edge_width", "边宽度", min = 0.1, max = 2, value = 0.8, step = 0.1),
              sliderInput("heb_label_size", "标签大小", min = 1, max = 3, value = 1.5, step = 0.1),
              checkboxInput("show_heb_labels", "显示节点标签", value = TRUE)
          )
        ),
        hr(),
        div(class = "sidebar-group",
            h3("下载图表"),
            downloadButton("downloadPDF", "下载 PDF", class = "download-button"),
            downloadButton("downloadSVG", "下载 SVG", class = "download-button"),
            downloadButton("downloadPNG", "下载 PNG", class = "download-button")
        )
    )
  )
}

# 主面板UI
main_panel_ui <- function() {
  mainPanel(
    class = "main",
    width = 9,
    tabsetPanel(
      id = "main_tabs",
      tabPanel("可视化图表",
               conditionalPanel(condition = "input.chart_type == 'Arc Diagram'",
                                h4(em("Arc Diagram 结果")),
                                jqui_resizable(plotOutput("arcDiagramPlot", width = "100%", height = "650px"))),
               conditionalPanel(condition = "input.chart_type == 'Chord Diagram'",
                                h4(em("Chord Diagram 结果")),
                                div(class = "plot-container",
                                    plotOutput("chordPlot", height = "700px"))
               ),
               conditionalPanel(condition = "input.chart_type == 'Sankey Diagram'",
                                h4(em("Sankey Diagram 结果")),
                                div(class = "plot-container",
                                    sankeyNetworkOutput("sankeyPlot", height = "700px"))
               ),
               conditionalPanel(condition = "input.chart_type == 'Hierarchical Edge Bundling'",
                                h4(em("Hierarchical Edge Bundling 结果")),
                                div(class = "plot-container",
                                    plotOutput("hebPlot", height = "700px"))
               ),
      ),
      tabPanel("数据预览",
               h4(textOutput("preview_title")),
               tableOutput("dataTablePreview"),
               verbatimTextOutput("edge_stats")
      ),
      tabPanel("使用说明",
               HTML("<h3>使用说明</h3>
                    <p><strong>数据格式要求:</strong> CSV为矩阵结构（第一列=行名/起点，第一行=列名/终点，中间为数值）。</p>
                    <p><strong>Arc Diagram:</strong> 未上传文件时自动使用随机生成矩阵，仅展示value>0且from≠to的边。</p>
                    <p><strong>Chord Diagram:</strong> 支持上传矩阵或随机生成数据，展示区域间的连接关系。</p>
                    <p><strong>Sankey Diagram:</strong> 支持上传矩阵或随机生成数据，展示有向流量关系。</p>
                    <p><strong>Hierarchical Edge Bundling:</strong> 展示层级节点间的连接关系，支持环形/树状布局。</p>
                    <p><strong>ggtree:</strong> 展示进化树结构，支持多种布局方式。</p>"))
    )
  )
}

# 完整UI组装函数
app_ui <- function() {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),  # 引用外部CSS
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
    ),
    title_ui(),  # 标题栏
    div(class = "loading-spinner", id = "loading-spinner", style = "display: none;",
        div(class = "spinner"),
        p("正在处理数据，请稍候...")
    ),
    tags$button(id = "toggle-sidebar", tags$i(class = "fa-solid fa-bars")),
    tags$button(id = "restore-sidebar", tags$i(class = "fa-solid fa-arrow-right-to-bracket")),
    sidebarLayout(
      sidebar_ui(),  # 侧边栏
      main_panel_ui()  # 主面板
    ),
    # JavaScript代码
    tags$script(HTML('
      function triggerResizeEvent() {
        setTimeout(function() {
          window.dispatchEvent(new Event("resize"));
        }, 100);
      }

      document.getElementById("toggle-sidebar").addEventListener("click", function() {
        var sidebar = document.querySelector(".sidebar");
        var main = document.querySelector(".main");
        var toggleButton = document.getElementById("toggle-sidebar");
        var restoreButton = document.getElementById("restore-sidebar");

        sidebar.classList.toggle("hidden");
        main.classList.toggle("full-width");

        if (sidebar.classList.contains("hidden")) {
          toggleButton.style.left = "0px";
        } else {
          toggleButton.style.left = "300px";
        }

        triggerResizeEvent();
      });

      document.getElementById("restore-sidebar").addEventListener("click", function() {
        var sidebar = document.querySelector(".sidebar");
        var main = document.querySelector(".main");
        var toggleButton = document.getElementById("toggle-sidebar");

        sidebar.classList.remove("hidden");
        main.classList.remove("full-width");
        toggleButton.style.left = "300px";

        triggerResizeEvent();
      });

      window.addEventListener("resize", function() {
        Shiny.onInputChange("window_resized", Date.now());
        updatePlotDimensions();
      });

      function updatePlotDimensions() {
        var plotContainer = document.querySelector(".plot-container");
        var mainPanel = document.querySelector(".main");
        
        if (!plotContainer) return;
        
        var containerWidth = mainPanel.offsetWidth;
        var containerHeight = mainPanel.offsetHeight;
        
        plotContainer.style.width = containerWidth + "px";
        plotContainer.style.height = containerHeight + "px";
      }

      document.addEventListener("DOMContentLoaded", function() {
        updatePlotDimensions();
        
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.attributeName === "class") {
              updatePlotDimensions();
            }
          });
        });
        
        observer.observe(document.querySelector(".main"), { attributes: true });
      });

      Shiny.addCustomMessageHandler("show_loading", function(message) {
        document.getElementById("loading-spinner").style.display = "flex";
      });

      Shiny.addCustomMessageHandler("hide_loading", function(message) {
        document.getElementById("loading-spinner").style.display = "none";
      });
    '))
  )
}