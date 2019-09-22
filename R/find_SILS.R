clip_matrix <- function(m, lower, upper) {
  clipped_m <- apply(m, 1, function(row) pmax(lower, pmin(row, upper)))
  rownames(clipped_m) <- colnames(clipped_m)
  clipped_m
}

find_SILS <- function(graph) {

  gc          <- find_geodetic_cycles(graph)
  edges_in_gc <- unique(unlist(gc))
  n_edges     <- length(edges_in_gc)

  B <- matrix(0, ncol = n_edges, nrow = n_edges,
              dimnames = list(edges_in_gc, edges_in_gc))

  # Adjacency matrix for each loop
  LA <- lapply(gc, function(cycle){
    aux_matrix   <- B
    loop_edges   <- c(cycle, cycle[1]) # Closes the loop
    n_iterations <- length(loop_edges) - 1

    for(i in 1:n_iterations) {

      edge_tail                        <- loop_edges[i]
      edge_head                        <- loop_edges[i + 1]
      aux_matrix[edge_tail, edge_head] <- 1

    }

    aux_matrix
  })

  SILS <- list()

  while(length(LA) > 0){

    # Number of contributing edges per loop
    n_cont_edges <- sapply(LA, function(adj_matrix) {
      sum(clip_matrix(adj_matrix - B, 0, 1))
    })

    non_contributory_loops <- n_cont_edges[n_cont_edges == 0]

    if(length(non_contributory_loops) > 0) {
      names_ncl     <- names(non_contributory_loops)
      LA[names_ncl] <- NULL # Removes non-contributory loops
      gc[names_ncl] <- NULL # Removes non-contributory loops
    }

    if(sum(n_cont_edges) > 0) {
      # If there are two loops or more, the first position is taken.
      # In the following iteration, the next position will be taken.
      min_cont    <- n_cont_edges[n_cont_edges == min(n_cont_edges)][1]
      name_mc     <- names(min_cont)
      B           <- clip_matrix(B + LA[[name_mc]], 0 , 1)
      SILS        <- c(SILS, gc[name_mc])
      LA[name_mc] <- NULL
      gc[name_mc] <- NULL
    }
  }

  SILS
}
