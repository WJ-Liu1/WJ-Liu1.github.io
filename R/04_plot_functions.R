# R/04_plot_functions.R
# Arc Diagram绘图函数
plot_arc_diagram <- function(data, input) {
  if (is.null(data)) return()
  par(mar = rep(0, 4) )
  par(cex = input$textSize / 5)
  
  edgelist <- as.matrix(as_edgelist(data$graph))
  node_sizes <- (log(data$degrees + 1) + 0.5) * input$node_size_multiplier
  arc_widths <- (data$values / max(data$values)) * input$arc_thickness_multiplier * 5 
  
  arcdiagram::arcplot(
    edgelist, 
    ordering = data$sorted_vlabels, 
    labels = data$vlabels, 
    cex.labels = 0.8,
    show.nodes = TRUE, 
    col.nodes = data$vborders,
    bg.nodes = data$vfill,
    cex.nodes = node_sizes, 
    pch.nodes = as.numeric(input$nodeShape),
    lwd.nodes = 2, 
    line = -0.5,
    col.arcs = input$lineColor, 
    lty.arcs = input$lineType,
    lwd.arcs = arc_widths,
    xpd = NA
  )
  if (input$addTitle) title(input$chartTitle)
}

# Chord Diagram绘图函数
plot_chord_diagram <- function(matrix_data, input) {
  circos.clear()
  
  if (is.null(matrix_data) || nrow(matrix_data) == 0 || ncol(matrix_data) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "无有效矩阵数据可绘制Chord图", cex = 1.2, col = "red")
    return()
  }
  
  data_long <- matrix_data %>%
    rownames_to_column(var = "from") %>%
    gather(key = "to", value = "value", -from) %>%
    filter(!is.na(value), value > 0, from != to)
  
  if (nrow(data_long) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Chord图无有效连接边！", cex = 1.2, col = "red")
    return()
  }
  
  all_sectors <- unique(c(data_long$from, data_long$to))
  n_colors <- length(all_sectors)
  if (input$color_scheme_chord == "RColorBrewer") {
    palette <- input$brewer_palette_chord
    max_brew <- brewer.pal.info[palette, "maxcolors"]
    mycolor <- brewer.pal(min(n_colors, max_brew), palette)
    mycolor <- rep(mycolor, length.out = n_colors)
  } else {
    mycolor <- viridis(n_colors, alpha = 1, option = input$color_scheme_chord)
  }
  names(mycolor) <- all_sectors
  
  circos.par(
    start.degree = 90, gap.degree = 4,
    track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE
  )
  par(mar = rep(0, 4))
  
  chordDiagram(
    x = data_long,
    grid.col = mycolor,
    transparency = input$transparency_chord,
    directional = ifelse(input$directional_chord, 1, 0),
    direction.type = c("arrows", "diffHeight"),
    diffHeight = -0.04 * input$direction_type_chord,
    annotationTrack = "grid",
    annotationTrackHeight = c(0.05, 0.1),
    link.arr.type = "big.arrow",
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    link.lwd = input$link_thickness_chord
  )
  
  circos.trackPlotRegion(
    track.index = 1, bg.border = NA,
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      sector.index = get.cell.meta.data("sector.index")
      text_cex <- input$text_size_chord * min(1, 10 / n_colors)
      circos.text(x = mean(xlim), y = 3.2, labels = sector.index,
                  facing = "bending", cex = max(0.5, text_cex), niceFacing = TRUE)
      max_val <- xlim[2]
      circos.axis(h = "top", major.at = seq(from = 0, to = max_val, by = max(1, floor(max_val / 4))),
                  minor.ticks = 1, major.tick.percentage = 0.5, labels.niceFacing = FALSE)
    }
  )
}

# HEB绘图函数（补充原代码中缺失的plot_heb_diagram）
plot_heb_diagram <- function(heb_data, input) {
  if (is.null(heb_data) || length(heb_data$from) < 2) {
    par(mar = rep(0, 4))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 2), ylim = c(0, 2))
    text(1, 1, "⚠️ 无有效数据可绘制HEB图", cex = 1.5, col = "#e74c3c", font = 2)
    text(1, 0.8, "请检查矩阵数据或调整随机参数", cex = 1, col = "#95a5a6")
    return()
  }
  
  heb_edge_width <- input$heb_edge_width %||% 0.4
  heb_bundling <- input$heb_bundling %||% 0.1
  heb_label_size <- input$heb_label_size %||% 1.5
  show_heb_labels <- input$show_heb_labels %||% TRUE
  
  p <- ggraph(heb_data$mygraph, layout = 'dendrogram', circular = TRUE) +
    geom_conn_bundle(
      data = get_con(from = heb_data$from, to = heb_data$to), 
      alpha = 0.1, 
      colour = "#69b3a2",
      width = heb_edge_width,
      tension = heb_bundling
    ) +
    {if(show_heb_labels) 
      geom_node_text(
        aes(x = x*1.01, y = y*1.01, filter = leaf, label = shortName, angle = angle, hjust = hjust),
        size = heb_label_size, 
        alpha = 1
      )
    } +
    coord_fixed() +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
  
  return(p)
}