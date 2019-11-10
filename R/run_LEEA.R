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

# Calculate edge elasticities
calculate_ee <- function(eigenvalue, graph, row, rx, lx, fx, gx) {

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
    edge_gain_expr <- rlang::parse_expr(edge$edge_gain)
    gain      <- with(row, eval(edge_gain_expr))

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

run_LEEA <- function (graph, sim_df, method = "analytical") {
  SILS             <- find_SILS(graph)
  cycle_matrix     <- as_cycle_matrix(graph, SILS)
  inv_cm           <- MASS::ginv(t(cycle_matrix)) # inverse cycle matrix
  rownames(inv_cm) <- rownames(cycle_matrix)
  colnames(inv_cm) <- colnames(cycle_matrix)

  if(method == "analytical") {
    #output from calculate loop gains (CLG) function
    output_clg <- calculate_loop_gains(graph)

    graph <- output_clg$graph # CLG added the 'edge_gains' attribute to edges

    # loop gains over time
    lg_over_time <- by(output_clg$loop_gains,
                       1:nrow(output_clg$loop_gains), function(loop_gain) {
                         data.frame(stringsAsFactors = FALSE,
                                    time = sim_df$time,
                                    loop_id = loop_gain$loop_id,
                                    gain = with(sim_df, eval(parse(text = loop_gain$gain))))
                       }) %>% purrr::map_df(function(row) row)

    levels    <-  names(igraph::V(graph)[[type == "stock"]])
    variables <- names(igraph::V(graph)[[type == "variable"]])
    n_levels  <- length(levels)
    n_vars    <- length(variables)

    # Matrix of gains for edges that connect two stocks
    Am <- matrix(0, nrow = n_levels, ncol = n_levels) # Node to node

    # Matrix of gains for flow
    Bm <- matrix(expression(0), nrow = n_levels, ncol = n_vars) # Flows
    rownames(Bm) <- levels
    colnames(Bm) <- variables

    flow_edges   <- igraph::E(graph)[[type == "flow"]]
    n_flows      <- length(flow_edges)

    for(i in 1:n_flows){
      flow      <- flow_edges[[i]]
      edge_head <- names(igraph::head_of(graph, flow))
      edge_tail <- names(igraph::tail_of(graph, flow))
      edge_gain <- rlang::parse_expr(flow$edge_gain)
      Bm[[edge_head, edge_tail]] <- edge_gain
    }

    # Matrix of gains for edges that connect a stock to a variable
    Cm <- matrix(expression(0), nrow = n_vars, ncol = n_levels)
    rownames(Cm) <- variables
    colnames(Cm) <- levels

    # Matrix of gains for edges that connect a variable to another variable
    Dm <- matrix(expression(0), nrow = n_vars, ncol = n_vars)
    rownames(Dm) <- variables
    colnames(Dm) <- variables

    info_edges   <- igraph::E(graph)[[type == "info_link"]]
    n_info_edges <- length(info_edges)

    for(i in 1:n_info_edges){
      info_edge <- info_edges[[i]]
      edge_head <- names(igraph::head_of(graph, info_edge))
      edge_tail <- names(igraph::tail_of(graph, info_edge))

      if(edge_tail %in% levels){ # Link from stock to variable
        edge_gain <- rlang::parse_expr(info_edge$edge_gain)
        Cm[[edge_head, edge_tail]] <- edge_gain
      }

      if(edge_tail %in% variables){ # Link from variable to variable
        edge_gain <- rlang::parse_expr(info_edge$edge_gain)
        Dm[[edge_head, edge_tail]] <- edge_gain
      }
    }

    leea_timestep <- by(sim_df, 1:nrow(sim_df), function(row, Am, Bm, Cm, Dm){

      evaluated_Am <- Am
      evaluated_Bm <- evaluate_matrix(Bm, row)
      evaluated_Cm <- evaluate_matrix(Cm, row)
      evaluated_Dm <- evaluate_matrix(Dm, row)
      Um           <- solve(diag(nrow(evaluated_Dm)) - evaluated_Dm)

      Jacobian     <- evaluated_Am + evaluated_Bm %*% Um %*% evaluated_Cm

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
        fx <- lx %*% evaluated_Bm %*% Um
        gx <- Um %*% evaluated_Cm %*% rx

        linkEla         <- calculate_ee(z, graph, row, rx, lx, fx, gx)
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

    }, Am = Am, Bm = Bm, Cm = Cm, Dm = Dm)

    eigenvalues_over_time <- purrr::map_df(leea_timestep, "eigenvalues")

    loop_analysis_over_time <- purrr::map_df(leea_timestep, "loop_analysis")


    return(
      list(loop_gains = lg_over_time,
           eigenvalues = eigenvalues_over_time,
           loop_analysis = loop_analysis_over_time,
           gains_matrices = list(A_matrix = Am,
                                 B_matrix = Bm,
                                 C_matrix = Cm,
                                 D_matrix = Dm),
           graph = graph)
    )
  }

  if(method == "numerical") {

    # lg_over_time <- calculate_loop_gains(graph, sim_df, sim_df$time,
    #                                      method = "numerical")
    return(
      list(loop_gains = "lg_over_time",
           eigenvalues = "eigenvalues_over_time",
           loop_analysis = "loop_analysis_over_time",
           gains_matrices = "list(A_matrix = Am,
                                 B_matrix = Bm,
                                 C_matrix = Cm,
                                 D_matrix = Dm)",
           graph = "graph")
    )
  }
}
