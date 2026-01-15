# R/03_server_modules.R
# 通用功能模块（帮助按钮、示例下载、数据预览）
general_server <- function(input, output, session, reactive_shared_data) {
  # 帮助按钮点击事件
  observeEvent(input$help_button, {
    showModal(modalDialog(
      title = "使用帮助",
      HTML("
        <h4>1. 数据准备</h4>
        <p>请准备矩阵格式的CSV文件：</p>
        <ul>
          <li>第一列：行名（节点/区域名称）</li>
          <li>第一行：列名（节点/区域名称）</li>
          <li>单元格：连接强度/流量数值</li>
        </ul>
        <h4>2. 操作步骤</h4>
        <ol>
          <li>选择图表类型</li>
          <li>选择数据来源（上传/随机生成）</li>
          <li>配置图表样式参数</li>
          <li>查看图表并下载</li>
        </ol>
        <h4>3. 注意事项</h4>
        <ul>
          <li>随机生成数据仅用于测试，建议上传自有数据</li>
          <li>Sankey图下载需要安装phantomjs（webshot2::install_phantomjs()）</li>
          <li>确保矩阵中无空值或非数值类型</li>
        </ul>
      "),
      easyClose = TRUE,
      footer = modalButton("关闭")
    ))
  })
  
  # 下载随机示例CSV
  output$downloadExample_mis <- downloadHandler(
    filename = function() {
      paste0(input$chart_type, "_example_", Sys.Date(), ".csv")
    },
    content = function(file) {
      example_mat <- switch(input$chart_type,
                            "Arc Diagram" = generate_random_arc_matrix(),
                            "Chord Diagram" = generate_random_chord_matrix(),
                            "Sankey Diagram" = generate_random_sankey_matrix(),
                            "Hierarchical Edge Bundling" = generate_random_HEB_matrix()$matrix,
                            generate_random_arc_matrix())
      write.csv(example_mat, file, row.names = TRUE, fileEncoding = "UTF-8")
    }
  )
  
  # 数据预览实现
  output$preview_title <- renderText({
    paste0(input$chart_type, " 数据预览")
  })
  
  output$dataTablePreview <- renderTable({
    req(reactive_shared_data())
    head(reactive_shared_data(), 10)
  })
}

# Arc Diagram Server模块
arc_server <- function(input, output, session, reactive_shared_data) {
  # 响应式读取数据（上传/随机）
  reactive_matrix_data_arc <- reactive({
    req(input$chart_type == "Arc Diagram")
    
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    
    if (input$arc_data_source == "upload" && !is.null(input$file_upload_arc) && input$file_upload_arc$size > 0) {
      tryCatch({
        matrix_data <- read.csv(
          file = input$file_upload_arc$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "UTF-8"
        )
      }, error = function(e) {
        matrix_data <- read.csv(
          file = input$file_upload_arc$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "GBK"
        )
      })
      
      if (!all(sapply(matrix_data, is.numeric))) {
        showNotification("上传数据包含非数值列！请检查矩阵格式", type = "error")
        return(NULL)
      }
      
      output$upload_status <- renderPrint({
        cat("✅ Arc Diagram 上传文件成功！\n")
        cat("文件名：", input$file_upload_arc$name, "\n")
        cat("数据维度：", dim(matrix_data), "\n")
      })
      
      edgelist <- reshape2::melt(as.matrix(matrix_data), varnames = c("from", "to"), value.name = "value", stringsAsFactors = FALSE) %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(edgelist)
      
      return(matrix_data)
      
    } else {
      random_mat <- generate_random_arc_matrix(
        n_nodes = input$n_nodes_arc %||% 8, 
        seed = input$seed_arc %||% 42
      )
      
      output$upload_status <- renderPrint({
        cat("ℹ Arc Diagram 使用随机生成矩阵\n")
        cat("节点数量：", input$n_nodes_arc %||% 8, "\n")
        cat("随机种子：", input$seed_arc %||% 42, "\n")
        cat("矩阵维度：", dim(random_mat), "\n")
      })
      
      edgelist <- reshape2::melt(as.matrix(random_mat), varnames = c("from", "to"), value.name = "value", stringsAsFactors = FALSE) %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(edgelist)
      
      return(random_mat)
    }
  })
  
  # 矩阵转边列表
  reactive_connect_df_arc <- reactive({
    req(reactive_matrix_data_arc())
    matrix_data <- reactive_matrix_data_arc()
    
    connect_df <- reshape2::melt(
      as.matrix(matrix_data), 
      varnames = c("from", "to"), 
      value.name = "value",
      stringsAsFactors = FALSE
    ) %>% 
      mutate(
        from = as.character(from),
        to = as.character(to)
      ) %>% 
      filter(!is.na(value), value > 0, from != to)
    
    output$edge_stats <- renderPrint({
      cat("Arc Diagram 有效边数量：", nrow(connect_df), "\n")
      if (nrow(connect_df) == 0) {
        cat("❌ 无有效边！请调整随机参数或上传数据\n")
      } else {
        cat("✅ from/to类型：", class(connect_df$from), "\n")
      }
    })
    
    return(connect_df)
  })
  
  # 网络构建
  reactive_arc_graph <- reactive({
    req(input$chart_type == "Arc Diagram", reactive_connect_df_arc(), nrow(reactive_connect_df_arc()) > 0)
    connect_df <- reactive_connect_df_arc()
    vfill_default <- input$pointColor
    
    temp_graph <- graph_from_data_frame(connect_df, directed = FALSE)
    com <- tryCatch(
      cluster_walktrap(temp_graph),
      error = function(e) { list(membership = rep(1, vcount(temp_graph))) }
    )
    
    nodes_df <- data.frame(
      name = V(temp_graph)$name, 
      grp = com$membership, 
      n = igraph::degree(temp_graph), 
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(grp), desc(n)) %>%
      filter(grp < 100)
    
    valid_nodes <- nodes_df$name
    connect_filtered <- connect_df %>%
      filter(from %in% valid_nodes, to %in% valid_nodes)
    graph_final <- graph_from_data_frame(connect_filtered, vertices = nodes_df, directed = FALSE)
    
    vcount_final <- vcount(graph_final)
    vfill <- if ("fill" %in% colnames(nodes_df)) nodes_df$fill else rep(vfill_default, vcount_final)
    vborders <- if ("border" %in% colnames(nodes_df)) nodes_df$border else rep("black", vcount_final)
    
    degrees_vec <- igraph::degree(temp_graph)
    values_vec <- E(graph_final)$value
    if (is.null(values_vec) || length(values_vec) == 0 || all(is.na(values_vec))) {
      values_vec <- rep(1, ecount(graph_final))
    }
    
    return(list(
      graph = graph_final,
      sorted_vlabels = as.character(nodes_df$name),
      degrees = degrees_vec,
      values = values_vec,
      vlabels = as.character(nodes_df$name),
      vfill = vfill,
      vborders = vborders
    ))
  })
  
  # Arc输出绘图
  output$arcDiagramPlot <- renderPlot({
    req(reactive_arc_graph())
    tryCatch({
      plot_arc_diagram(reactive_arc_graph(), input)
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Arc绘图失败：", e$message), cex = 1.2, col = "red")
    })
  })
  
  # Arc随机数据刷新
  observeEvent(input$refresh_data_arc, {
    req(input$chart_type == "Arc Diagram")
    showNotification("Arc Diagram 随机矩阵已重新生成", type = "message")
  })
}

# Chord Diagram Server模块
chord_server <- function(input, output, session, reactive_shared_data) {
  # Chord响应式矩阵数据
  reactive_chord_matrix <- reactive({
    req(input$chart_type == "Chord Diagram")
    
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    
    data_source <- input$chord_data_source %||% "random"
    
    if (data_source == "upload" && !is.null(input$file_upload_chord) && input$file_upload_chord$size > 0) {
      tryCatch({
        matrix_data <- read.csv(
          file = input$file_upload_chord$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "UTF-8"
        )
      }, error = function(e) {
        matrix_data <- read.csv(
          file = input$file_upload_chord$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "GBK"
        )
      })
      
      if (!all(sapply(matrix_data, is.numeric))) {
        showNotification("Chord上传数据包含非数值列！请检查矩阵格式", type = "error")
        return(NULL)
      }
      
      diag(matrix_data) <- 0
      row_sums <- rowSums(matrix_data, na.rm = TRUE)
      col_sums <- colSums(matrix_data, na.rm = TRUE)
      keep_rows <- row_sums > 0
      keep_cols <- col_sums > 0
      
      if (sum(keep_rows) == 0 || sum(keep_cols) == 0) {
        showNotification("Chord矩阵无有效数据！", type = "warning")
        return(NULL)
      }
      
      final_matrix <- matrix_data[keep_rows | keep_cols, keep_rows | keep_cols, drop = FALSE]
      
      output$upload_status <- renderPrint({
        cat("✅ Chord Diagram 上传矩阵成功！\n")
        cat("文件名：", input$file_upload_chord$name, "\n")
        cat("原始矩阵维度：", dim(matrix_data), "\n")
        cat("清洗后矩阵维度：", dim(final_matrix), "\n")
      })
      
      edgelist <- final_matrix %>%
        rownames_to_column(var = "from") %>%
        gather(key = "to", value = "value", -from) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(edgelist)
      
      return(final_matrix)
      
    } else {
      random_mat <- generate_random_chord_matrix(
        n_regions = input$n_regions_chord %||% 10, 
        seed = input$seed_chord %||% 42
      )
      
      row_sums <- rowSums(random_mat, na.rm = TRUE)
      col_sums <- colSums(random_mat, na.rm = TRUE)
      keep_rows <- row_sums > 0
      keep_cols <- col_sums > 0
      final_matrix <- random_mat[keep_rows | keep_cols, keep_rows | keep_cols, drop = FALSE]
      
      if (nrow(final_matrix) == 0) {
        final_matrix <- generate_random_chord_matrix(n_regions = 5, seed = 42)
      }
      
      output$upload_status <- renderPrint({
        cat("ℹ Chord Diagram 使用随机生成矩阵\n")
        cat("区域数量：", input$n_regions_chord %||% 10, "\n")
        cat("随机种子：", input$seed_chord %||% 42, "\n")
        cat("矩阵维度：", dim(final_matrix), "\n")
      })
      
      edgelist <- final_matrix %>%
        rownames_to_column(var = "from") %>%
        gather(key = "to", value = "value", -from) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(edgelist)
      
      return(final_matrix)
    }
  })
  
  # Chord随机数据刷新
  observeEvent(input$refresh_data_chord, {
    req(input$chart_type == "Chord Diagram", 
        !is.na(input$chord_data_source), 
        input$chord_data_source == "random")
    showNotification("Chord Diagram 随机矩阵已重新生成", type = "message")
  })
  
  # Chord输出绘图
  output$chordPlot <- renderPlot({
    req(reactive_chord_matrix())
    tryCatch({
      plot_chord_diagram(reactive_chord_matrix(), input)
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Chord绘图失败：", e$message), cex = 1.2, col = "red")
    })
  }, res = 96)
}

# Sankey Diagram Server模块
sankey_server <- function(input, output, session, reactive_shared_data) {
  # Sankey响应式矩阵数据
  reactive_sankey_matrix <- reactive({
    req(input$chart_type == "Sankey Diagram")
    
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    
    data_source <- input$sankey_data_source %||% "random"
    
    if (data_source == "upload" && !is.null(input$file_upload_sankey) && input$file_upload_sankey$size > 0) {
      tryCatch({
        matrix_data <- read.csv(
          file = input$file_upload_sankey$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "UTF-8"
        )
      }, error = function(e) {
        matrix_data <- read.csv(
          file = input$file_upload_sankey$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "GBK"
        )
      })
      
      if (!all(sapply(matrix_data, is.numeric))) {
        showNotification("Sankey上传数据包含非数值列！请检查矩阵格式", type = "error")
        return(NULL)
      }
      
      diag(matrix_data) <- 0
      row_sums <- rowSums(matrix_data, na.rm = TRUE)
      col_sums <- colSums(matrix_data, na.rm = TRUE)
      keep_rows <- row_sums > 0
      keep_cols <- col_sums > 0
      
      if (sum(keep_rows) == 0 || sum(keep_cols) == 0) {
        showNotification("Sankey矩阵无有效数据！", type = "warning")
        return(NULL)
      }
      
      final_matrix <- matrix_data[keep_rows | keep_cols, keep_rows | keep_cols, drop = FALSE]
      
      output$upload_status <- renderPrint({
        cat("✅ Sankey Diagram 上传矩阵成功！\n")
        cat("文件名：", input$file_upload_sankey$name, "\n")
        cat("原始矩阵维度：", dim(matrix_data), "\n")
        cat("清洗后矩阵维度：", dim(final_matrix), "\n")
      })
      
      edgelist <- reshape2::melt(as.matrix(final_matrix), varnames = c("from", "to"), value.name = "value", stringsAsFactors = FALSE) %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(edgelist)
      
      return(final_matrix)
      
    } else {
      random_mat <- generate_random_sankey_matrix(
        n_regions = input$n_regions_sankey %||% 10, 
        seed = input$seed_sankey %||% 42
      )
      
      row_sums <- rowSums(random_mat, na.rm = TRUE)
      col_sums <- colSums(random_mat, na.rm = TRUE)
      keep_rows <- row_sums > 0
      keep_cols <- col_sums > 0
      final_matrix <- random_mat[keep_rows | keep_cols, keep_rows | keep_cols, drop = FALSE]
      
      if (nrow(final_matrix) == 0) {
        final_matrix <- generate_random_sankey_matrix(n_regions = 5, seed = 42)
      }
      
      output$upload_status <- renderPrint({
        cat("ℹ Sankey Diagram 使用随机生成矩阵\n")
        cat("区域数量：", input$n_regions_sankey %||% 10, "\n")
        cat("随机种子：", input$seed_sankey %||% 42, "\n")
        cat("矩阵维度：", dim(final_matrix), "\n")
      })
      
      edgelist <- reshape2::melt(as.matrix(final_matrix), varnames = c("from", "to"), value.name = "value", stringsAsFactors = FALSE) %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(edgelist)
      
      return(final_matrix)
    }
  })
  
  # Sankey数据格式转换
  reactive_sankey_data <- reactive({
    req(reactive_sankey_matrix())
    matrix_data <- reactive_sankey_matrix()
    
    data_long <- reshape2::melt(as.matrix(matrix_data), varnames = c("from", "to"), value.name = "value", stringsAsFactors = FALSE) %>%
      mutate(from = as.character(from), to = as.character(to)) %>%
      filter(!is.na(value), value > 0, from != to)
    
    if (nrow(data_long) == 0) {
      showNotification("Sankey图无有效连接边！", type = "warning")
      return(NULL)
    }
    
    all_nodes <- unique(c(data_long$from, data_long$to))
    nodes_df <- data.frame(
      name = all_nodes,
      node_id = 0:(length(all_nodes) - 1),
      stringsAsFactors = FALSE
    )
    
    links_df <- data_long %>%
      left_join(nodes_df %>% select(name, source = node_id), by = c("from" = "name")) %>%
      left_join(nodes_df %>% select(name, target = node_id), by = c("to" = "name")) %>%
      select(source, target, value) %>%
      filter(!is.na(source) & !is.na(target))
    
    return(list(nodes = nodes_df, links = links_df))
  })
  
  # Sankey图对象（用于下载）
  reactive_sankey_plot <- reactive({
    req(reactive_sankey_data())
    sankey_data <- reactive_sankey_data()
    
    if (is.null(sankey_data) || nrow(sankey_data$links) == 0) {
      return(
        sankeyNetwork(
          Links = data.frame(source = 0, target = 1, value = 1),
          Nodes = data.frame(name = c("无数据", "无数据")),
          Source = "source", Target = "target", Value = "value", NodeID = "name",
          fontSize = 12, nodeWidth = 10, unit = "单位"
        ) %>% 
          htmlwidgets::onRender("function(el, x) {
          d3.select(el).select('svg').append('text')
            .attr('x', x.width/2).attr('y', x.height/2)
            .attr('text-anchor', 'middle').text('无有效数据可绘制Sankey图')
            .style('font-size', '16px').style('fill', 'red');
        }")
      )
    }
    
    color_scale <- paste0(
      "d3.scaleOrdinal().range(['", stringr::str_trim(input$sankey_color), "'])",
      if (nrow(sankey_data$nodes) > 0) {
        paste0(".domain(d3.range(", nrow(sankey_data$nodes), "))")
      }
    )
    
    sankey_plot <- sankeyNetwork(
      Links = sankey_data$links,
      Nodes = sankey_data$nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = input$sankey_text_size,
      nodeWidth = input$sankey_node_width,
      unit = input$sankey_unit,
      colourScale = JS(color_scale),
      iterations = input$sankey_iterations
    )
    
    if (input$sankey_node_shadow) {
      sankey_plot <- sankey_plot %>%
        htmlwidgets::onRender("function(el, x) {
        d3.select(el).selectAll('.node rect')
          .style('filter', 'drop-shadow(2px 2px 3px rgba(0,0,0,0.3))')
          .select('text')
          .style('text-shadow', '1px 1px 2px rgba(255,255,255,0.8)');
      }")
    }
    
    return(sankey_plot)
  })
  
  # Sankey随机数据刷新
  observeEvent(input$refresh_data_sankey, {
    req(input$chart_type == "Sankey Diagram", 
        !is.na(input$sankey_data_source), 
        input$sankey_data_source == "random")
    showNotification("Sankey Diagram 随机矩阵已重新生成", type = "message")
  })
  
  # Sankey绘图输出
  output$sankeyPlot <- renderSankeyNetwork({
    reactive_sankey_plot()
  })
}

# HEB Server模块
heb_server <- function(input, output, session, reactive_shared_data) {
  # HEB响应式矩阵数据
  reactive_heb_matrix <- reactive({
    req(input$chart_type == "Hierarchical Edge Bundling")
    
    data_source <- input$heb_data_source %||% "random"
    
    if (data_source == "upload" && !is.null(input$file_upload_heb) && input$file_upload_heb$size > 0) {
      tryCatch({
        matrix_data <- read.csv(
          file = input$file_upload_heb$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "UTF-8"
        )
      }, error = function(e) {
        matrix_data <- read.csv(
          file = input$file_upload_heb$datapath,
          header = TRUE,
          row.names = 1,
          stringsAsFactors = FALSE,
          encoding = "GBK"
        )
      })
      
      if (!all(sapply(matrix_data, is.numeric))) {
        showNotification("HEB上传数据包含非数值列！", type = "error")
        return(NULL)
      }
      
      diag(matrix_data) <- 0
      row_sums <- rowSums(matrix_data, na.rm = TRUE)
      col_sums <- colSums(matrix_data, na.rm = TRUE)
      keep_rows <- row_sums > 0
      keep_cols <- col_sums > 0
      
      if (sum(keep_rows) == 0 || sum(keep_cols) == 0) {
        showNotification("HEB矩阵无有效数据！", type = "warning")
        return(NULL)
      }
      
      final_matrix <- matrix_data[keep_rows | keep_cols, keep_rows | keep_cols, drop = FALSE]
      
      output$upload_status <- renderPrint({
        cat("✅ HEB 上传矩阵成功！\n")
        cat("文件名：", input$file_upload_heb$name, "\n")
        cat("矩阵维度：", dim(final_matrix), "\n")
      })
      
      final_edgelist <- reshape2::melt(as.matrix(final_matrix), 
                                       varnames = c("from", "to"), 
                                       value.name = "value", 
                                       stringsAsFactors = FALSE) %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(final_edgelist)
      
      return(final_matrix)
      
    } else {
      random_heb_data <- generate_random_HEB_matrix(
        n_regions = input$n_regions_heb %||% 10, 
        seed = input$seed_heb %||% 42
      )
      
      final_matrix <- random_heb_data$matrix
      
      output$upload_status <- renderPrint({
        cat("ℹ HEB 使用随机生成矩阵\n")
        cat("节点数量：", input$n_regions_heb %||% 10, "\n")
        cat("随机种子：", input$seed_heb %||% 42, "\n")
        cat("矩阵维度：", dim(final_matrix), "\n")
      })
      
      final_edgelist <- reshape2::melt(as.matrix(final_matrix), 
                                       varnames = c("from", "to"), 
                                       value.name = "value", 
                                       stringsAsFactors = FALSE) %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(!is.na(value), value > 0, from != to)
      reactive_shared_data(final_edgelist)
      
      return(final_matrix)
    }
  })
  
  # HEB图的响应式逻辑
  reactive_heb_graph <- reactive({
    req(input$chart_type == "Hierarchical Edge Bundling", reactive_heb_matrix())
    
    matrix_data <- reactive_heb_matrix()
    
    final_edgelist <- reshape2::melt(as.matrix(matrix_data), 
                                     varnames = c("from", "to"), 
                                     value.name = "value", 
                                     stringsAsFactors = FALSE) %>%
      mutate(from = as.character(from), to = as.character(to)) %>%
      filter(!is.na(value), value > 0, from != to)
    
    all_nodes <- unique(c(final_edgelist$from, final_edgelist$to))
    root_node <- "Root"
    all_nodes <- c(root_node, all_nodes)
    
    vertices <- data.frame(
      name = all_nodes,
      stringsAsFactors = FALSE
    ) %>% 
      arrange(name) %>% 
      mutate(name = factor(name, levels = name)) %>%
      mutate(shortName = ifelse(name == root_node, root_node, gsub("^.*_", "", name)))
    
    edges <- data.frame(
      from = rep(root_node, length(all_nodes)-1),
      to = all_nodes[all_nodes != root_node],
      stringsAsFactors = FALSE
    )
    
    connections <- final_edgelist %>% select(from, to)
    
    vertices$id <- NA
    myleaves <- which(is.na(match(vertices$name, edges$from)))
    nleaves <- length(myleaves)
    vertices$id[myleaves] <- seq(1:nleaves)
    vertices$angle <- 90 - 360 * vertices$id / nleaves
    vertices$hjust <- ifelse(vertices$angle < -90, 1, 0)
    vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)
    
    mygraph <- graph_from_data_frame(edges, vertices = vertices)
    
    from <- match(connections$from, vertices$name)
    to <- match(connections$to, vertices$name)
    valid_idx <- !is.na(from) & !is.na(to)
    from <- from[valid_idx]
    to <- to[valid_idx]
    
    if (length(from) < 2) {
      showNotification("HEB 需要至少 2 条有效边才能绘制", type = "error")
      return(NULL)
    }
    
    return(list(
      mygraph = mygraph,
      vertices = vertices,
      from = from,
      to = to,
      nleaves = nleaves,
      connections = connections[valid_idx, ],
      matrix = matrix_data
    ))
  })
  
  # HEB随机数据刷新
  observeEvent(input$refresh_data_heb, {
    req(input$chart_type == "Hierarchical Edge Bundling")
    invalidateLater(100)
    showNotification("HEB 随机矩阵已重新生成", type = "message")
  })
  
  # HEB绘图输出
  output$hebPlot <- renderPlot({
    req(input$chart_type == "Hierarchical Edge Bundling", reactive_heb_graph())
    heb_data <- reactive_heb_graph()
    
    p <- plot_heb_diagram(heb_data, input)
    return(p)
  }, res = 96, execOnResize = FALSE)
}

# 下载功能模块
download_server <- function(input, output, session) {
  # 下载PDF格式
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$chart_type), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      session$sendCustomMessage("show_loading", list())
      on.exit(session$sendCustomMessage("hide_loading", list()))
      
      if (input$chart_type == "Arc Diagram") {
        pdf(file, width = 12, height = 8)
        plot_arc_diagram(reactive_arc_graph(), input)
        dev.off()
      } else if (input$chart_type == "Chord Diagram") {
        pdf(file, width = 10, height = 10)
        plot_chord_diagram(reactive_chord_matrix(), input)
        dev.off()
      } else if (input$chart_type == "Sankey Diagram") {
        temp_html <- tempfile(fileext = ".html")
        saveWidget(reactive_sankey_plot(), temp_html, selfcontained = TRUE)
        webshot2::webshot(temp_html, file, vwidth = 1200, vheight = 800, delay = 1)
      } else if (input$chart_type == "Hierarchical Edge Bundling") {
        pdf(file, width = 10, height = 10)
        p <- plot_heb_diagram(reactive_heb_graph(), input)
        print(p)
        dev.off()
      }
    }
  )
  
  # 下载SVG格式
  output$downloadSVG <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$chart_type), "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      session$sendCustomMessage("show_loading", list())
      on.exit(session$sendCustomMessage("hide_loading", list()))
      
      if (input$chart_type == "Arc Diagram") {
        svg(file, width = 12, height = 8)
        plot_arc_diagram(reactive_arc_graph(), input)
        dev.off()
      } else if (input$chart_type == "Chord Diagram") {
        svg(file, width = 10, height = 10)
        plot_chord_diagram(reactive_chord_matrix(), input)
        dev.off()
      } else if (input$chart_type == "Sankey Diagram") {
        temp_html <- tempfile(fileext = ".html")
        saveWidget(reactive_sankey_plot(), temp_html, selfcontained = TRUE)
        webshot2::webshot(temp_html, file, vwidth = 1200, vheight = 800, format = "svg")
      } else if (input$chart_type == "Hierarchical Edge Bundling") {
        svg(file, width = 10, height = 10)
        p <- plot_heb_diagram(reactive_heb_graph(), input)
        print(p)
        dev.off()
      }
    }
  )
  
  # 下载PNG格式
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$chart_type), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      session$sendCustomMessage("show_loading", list())
      on.exit(session$sendCustomMessage("hide_loading", list()))
      
      if (input$chart_type == "Arc Diagram") {
        png(file, width = 1200, height = 800, res = 100)
        plot_arc_diagram(reactive_arc_graph(), input)
        dev.off()
      } else if (input$chart_type == "Chord Diagram") {
        png(file, width = 1000, height = 1000, res = 100)
        plot_chord_diagram(reactive_chord_matrix(), input)
        dev.off()
      } else if (input$chart_type == "Sankey Diagram") {
        temp_html <- tempfile(fileext = ".html")
        saveWidget(reactive_sankey_plot(), temp_html, selfcontained = TRUE)
        webshot2::webshot(temp_html, file, vwidth = 1200, vheight = 800, delay = 2)
      } else if (input$chart_type == "Hierarchical Edge Bundling") {
        png(file, width = 1000, height = 1000, res = 100)
        p <- plot_heb_diagram(reactive_heb_graph(), input)
        print(p)
        dev.off()
      }
    }
  )
}

# 完整Server组装函数
app_server <- function(input, output, session) {
  # 共享响应式数据
  reactive_shared_data <- reactiveVal(NULL)
  
  # 加载各模块
  general_server(input, output, session, reactive_shared_data)
  arc_server(input, output, session, reactive_shared_data)
  chord_server(input, output, session, reactive_shared_data)
  sankey_server(input, output, session, reactive_shared_data)
  heb_server(input, output, session, reactive_shared_data)
  download_server(input, output, session)
}