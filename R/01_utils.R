# R/01_utils.R
# 兜底运算符：支持标量+向量，避免||处理多长度逻辑值
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    y
  } else {
    x
  }
}

# 生成Arc图随机矩阵（适配Arc图逻辑）
generate_random_arc_matrix <- function(n_nodes = 8, seed = 42) {
  set.seed(seed)
  random_mat <- matrix(sample(0:10, size = n_nodes*n_nodes, replace = TRUE), 
                       nrow = n_nodes, ncol = n_nodes)
  random_mat <- (random_mat + t(random_mat)) / 2
  diag(random_mat) <- 0
  rownames(random_mat) <- colnames(random_mat) <- paste0("Node_", 1:n_nodes)
  random_mat <- as.data.frame(random_mat)
  return(random_mat)
}

# 生成Chord图随机矩阵
generate_random_chord_matrix <- function(n_regions = 10, seed = 42) {
  set.seed(seed)
  random_mat <- matrix(sample(0:20, size = n_regions*n_regions, replace = TRUE), 
                       nrow = n_regions, ncol = n_regions)
  diag(random_mat) <- 0
  rownames(random_mat) <- colnames(random_mat) <- paste0("Region_", 1:n_regions)
  random_mat <- as.data.frame(random_mat)
  return(random_mat)
}

# 生成Sankey图随机矩阵（专属辅助函数）
generate_random_sankey_matrix <- function(n_regions = 10, seed = 42) {
  set.seed(seed)
  random_mat <- matrix(sample(0:50, size = n_regions*n_regions, replace = TRUE), 
                       nrow = n_regions, ncol = n_regions)
  diag(random_mat) <- 0
  rownames(random_mat) <- paste0("Source_", 1:n_regions)
  colnames(random_mat) <- paste0("Target_", 1:n_regions)
  random_mat <- as.data.frame(random_mat)
  return(random_mat)
}

# 生成符合flare格式的随机HEB数据
generate_random_HEB_matrix <- function(n_regions = 10, seed = 42) {
  set.seed(seed)
  root_node <- "Root"
  leaf_nodes <- paste0("Node_", 1:n_regions)
  all_nodes <- c(root_node, leaf_nodes)
  
  edges <- data.frame(
    from = rep(root_node, n_regions),
    to = leaf_nodes,
    stringsAsFactors = FALSE
  )
  
  vertices <- data.frame(
    name = all_nodes,
    stringsAsFactors = FALSE
  ) %>% 
    arrange(name) %>% 
    mutate(name = factor(name, levels = name)) %>%
    mutate(
      shortName = ifelse(name == root_node, root_node, gsub("Node_", "", name)),
      id = NA,
      angle = NA,
      hjust = NA
    )
  
  imports <- data.frame(
    from = sample(leaf_nodes, n_regions*2, replace = TRUE),
    to = sample(leaf_nodes, n_regions*2, replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    filter(from != to) %>%
    distinct()
  
  node_names <- leaf_nodes
  random_mat <- matrix(0, nrow = length(node_names), ncol = length(node_names), dimnames = list(node_names, node_names))
  for (i in 1:nrow(imports)) {
    from_node <- imports$from[i]
    to_node <- imports$to[i]
    random_mat[from_node, to_node] <- 1
  }
  random_mat <- as.data.frame(random_mat)
  
  return(list(
    matrix = random_mat,
    edges = edges,
    vertices = vertices,
    imports = imports
  ))
}