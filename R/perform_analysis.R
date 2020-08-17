

# Calculate edge elasticities
calculate_ee <- function(eigenvalue, graph, row, rx, lx, fx, gx,
                         method = "analytical", graph_funs) {

  n_edges      <- igraph::gsize(graph)

  if(eigenvalue == 0) return(rep(0, n_edges))

  edges        <- igraph::E(graph)
  n_edges      <- igraph::gsize(graph)
  elasticities <- vector(length = n_edges)
  levels       <-  names(igraph::V(graph)[[type == "stock"]])
  variables    <- names(igraph::V(graph)[[type == "variable"]])

  for(i in 1:n_edges) {
    edge      <- edges[[i]]
    edge_tail <- names(igraph::tail_of(graph, edge))
    edge_head <- names(igraph::head_of(graph, edge))
    edge_type <- edge$type

    if(method == "analytical") {
      edge_gain_expr <- rlang::parse_expr(edge$edge_gain)
      gain           <- with(row, eval(edge_gain_expr))
    }

    if(method == "numerical") {
      gain <- approx_edge_gain(graph, edge_head, edge_tail, row, graph_funs)
    }


    if(edge_type == "flow") {
      elasticities[i] <- lx[which(levels == edge_head)] * gx[edge_tail, 1] * gain / eigenvalue
    }

    if(edge_type == "info_link" && edge_tail %in% levels) {
      elasticities[i] <- fx[1, edge_head] * rx[which(levels == edge_tail)] * gain / eigenvalue
    }

    if(edge_type == "info_link" && edge_tail %in% variables) {
      elasticities[i] <- fx[1, edge_head] * gx[edge_tail, 1] * gain / eigenvalue
    }
  }

  elasticities
}

as_cycle_matrix <- function(graph, loops) {
  edge_list  <- igraph::as_edgelist(graph)
  edges      <- paste(edge_list[ , 1], edge_list[ , 2], sep = "|")
  loop_names <- names(loops)

  cycle_matrix <- matrix(0, nrow = length(loops),
                         ncol = nrow(edge_list),
                         dimnames = list(loop_names, edges))

  for(i in 1:length(loops)){
    loop          <- loops[[i]]
    edges_in_loop <- c(loop, loop[1]) # closes the loop
    n_j_iter      <- length(edges_in_loop) - 1 # number of j iterations

    for(j in 1:n_j_iter) {
      edge <- paste(edges_in_loop[j], edges_in_loop[j + 1], sep = "|")
      cycle_matrix[i, edge] <- 1
    }
  }
  cycle_matrix
}

#' @param Am Matrix of gains for edges that connect two stocks
#' @param Bm Matrix of gains for flows
#' @param Cm Matrix of gains for edges that connect a stock to a variable
#' @param Dm Matrix of gains for edges that connect a variable to another variable
#' @param graph A graph
#' @param row A row
#' @param inv_cm Inverse cycle matrix
#' @param n_levels Number of levels
#' @param method A string
#' @return A list with two dataframes.
perform_analysis <- function(Am, Bm, Cm, Dm, graph, row, inv_cm, n_levels,
                             method, graph_funs = NULL) {
  Um           <- solve(diag(nrow(Dm)) - Dm)

  Jacobian     <- Am + Bm %*% Um %*% Cm

  eigensystem        <- eigen(Jacobian)
  eigenvalues        <- eigensystem$values
  ev_ids             <- paste0("EV", 1:length(eigenvalues))
  names(eigenvalues) <- ev_ids

  eigenvectors           <- eigensystem$vectors
  rownames(eigenvectors) <- ev_ids

  Ln           <- solve(t(eigenvectors))
  Ln           <- zapsmall(Ln)

  eigenvalues_df <- data.frame(time = row$time,
                               eigenvalue = ev_ids,
                               real_value = Re(eigenvalues),
                               imaginary_value = Im(eigenvalues))

  loop_analysis_df <- purrr::map_df(1:n_levels, function(i){
    z  <- eigenvalues[i]
    rx <- eigenvectors[ ,i]
    lx <- Ln[, i]
    fx <- lx %*% Bm %*% Um
    gx <- Um %*% Cm %*% rx

    linkEla         <- calculate_ee(z, graph, row, rx, lx, fx, gx, method,
                                    graph_funs)

    loop_elasticity <- inv_cm %*% linkEla
    loop_influence  <- loop_elasticity * z

    data.frame(eigenvalue_id       = names(z),
               time                = row$time,
               loop_id             = rownames(loop_elasticity),
               loop_influence_Re   = Re(loop_influence[ , 1]),
               loop_influence_Im   = Im(loop_influence[ , 1]),
               loop_influence_abs  = abs(loop_influence[ , 1]),
               stringsAsFactors = F)
  })

  list(eigenvalues = eigenvalues_df, loop_analysis = loop_analysis_df)
}



