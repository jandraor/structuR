LEEA_numerical <- function(graph, sim_df, graph_funs) {
  SILS             <- find_SILS(graph)
  cycle_matrix     <- as_cycle_matrix(graph, SILS)
  inv_cm           <- MASS::ginv(t(cycle_matrix)) # inverse cycle matrix
  rownames(inv_cm) <- rownames(cycle_matrix)
  colnames(inv_cm) <- colnames(cycle_matrix)

  levels    <- names(igraph::V(graph)[[type == "stock"]])
  variables <- names(igraph::V(graph)[[type == "variable"]])
  n_levels  <- length(levels)
  n_vars    <- length(variables)

  lg_over_time <- calculate_loop_gains(graph, sim_df, sim_df$time,
                                       method = "numerical", graph_funs)

  leea_timestep_num <- by(sim_df, 1:nrow(sim_df), function(row){

    # Matrix of gains for edges that connect two stocks
    Am <- matrix(0, nrow = n_levels, ncol = n_levels) # Node to node

    # Matrix of gains for flows
    Bm <- matrix(0, nrow = n_levels, ncol = n_vars) # Flows
    rownames(Bm) <- levels
    colnames(Bm) <- variables

    flow_edges   <- igraph::E(graph)[[type == "flow"]]
    n_flows      <- length(flow_edges)

    for(i in 1:n_flows){
      flow      <- flow_edges[[i]]
      edge_head <- names(igraph::head_of(graph, flow))
      edge_tail <- names(igraph::tail_of(graph, flow))
      edge_gain <- approx_edge_gain(graph, edge_head, edge_tail, row)
      Bm[[edge_head, edge_tail]] <- edge_gain
    }

    # Matrix of gains for edges that connect a stock to a variable
    Cm <- matrix(0, nrow = n_vars, ncol = n_levels)
    rownames(Cm) <- variables
    colnames(Cm) <- levels

    # Matrix of gains for edges that connect a variable to another variable
    Dm <- matrix(0, nrow = n_vars, ncol = n_vars)
    rownames(Dm) <- variables
    colnames(Dm) <- variables

    info_edges   <- igraph::E(graph)[[type == "info_link"]]
    n_info_edges <- length(info_edges)

    for(i in 1:n_info_edges){
      info_edge <- info_edges[[i]]
      edge_head <- names(igraph::head_of(graph, info_edge))
      edge_tail <- names(igraph::tail_of(graph, info_edge))

      if(edge_tail %in% levels){ # Link from stock to variable
        edge_gain <- approx_edge_gain(graph, edge_head, edge_tail, row,
                                      graph_funs)
        Cm[[edge_head, edge_tail]] <- edge_gain
      }

      if(edge_tail %in% variables){ # Link from variable to variable
        edge_gain <- approx_edge_gain(graph, edge_head, edge_tail, row,
                                      graph_funs)
        Dm[[edge_head, edge_tail]] <- edge_gain
      }
    }

    perform_analysis(Am, Bm, Cm, Dm, graph, row, inv_cm, n_levels, "numerical",
                     graph_funs)
  })

  eigenvalues_over_time   <- purrr::map_df(leea_timestep_num, "eigenvalues")
  loop_analysis_over_time <- purrr::map_df(leea_timestep_num, "loop_analysis")

  list(loop_gains = lg_over_time,
       eigenvalues = eigenvalues_over_time,
       loop_analysis = loop_analysis_over_time,
       gains_matrices = "list(A_matrix = Am,
                              B_matrix = Bm,
                              C_matrix = Cm,
                              D_matrix = Dm)",
       graph = "graph")
}
