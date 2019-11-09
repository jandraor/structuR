delete.NULLs  <-  function(x.list){   # delete null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

#'  @export
find_geodetic_cycles <- function(graph) {
  strong_components   <- igraph::components(graph, mode = "strong")

  members <- igraph::membership(igraph::clusters(graph, mode = "strong"))

  strong_subgraphs <- lapply(unique(members), gr = graph,
                          function (x, gr){
                            igraph::induced.subgraph(gr, which(members == x))
                          })


  geodetic_cycles <- lapply(strong_subgraphs, function(strong_sgr) {
    n_nodes <- igraph::gorder(strong_sgr)

    if(n_nodes == 1) return (NULL)

    distance_matrix <- igraph::distances(strong_sgr, mode = "out")

    nodes <- colnames(distance_matrix)

    aux_dis_m                       <- distance_matrix
    aux_dis_m[upper.tri(aux_dis_m)] <- 0
    lower_triangular_matrix         <- aux_dis_m

    aux_dis_m                       <- distance_matrix
    aux_dis_m[lower.tri(aux_dis_m)] <- 0
    upper_triangular_matrix         <- aux_dis_m
    transposed_utm                  <- t(upper_triangular_matrix)

    loop_matrix <- transposed_utm + lower_triangular_matrix
    loop_lengths <- sort(unique(as.vector(loop_matrix)))
    loop_lengths <- loop_lengths[loop_lengths >= 2]


    # Loops in strong subgraph
    loops_in_ss <- list()

    for(loop_length in loop_lengths) {
      pairs <- which(loop_matrix == loop_length, arr.ind = TRUE)

      for(i in seq_len(nrow(pairs))) {
        u <- nodes[pairs[i, 1]]
        v <- nodes[pairs[i, 2]]

        paths_u_v <- igraph::all_simple_paths(graph, from = u, to = v)
        paths_v_u <- igraph::all_simple_paths(graph, from = v, to = u)

        paths_permutation <- purrr::cross2(paths_u_v, paths_v_u)

        purrr::walk(paths_permutation, function(permutation) {
          path_u_v  <- names(permutation[[1]])
          path_v_u <- names(permutation[[2]])

          path_back <- NULL
          if(length(path_v_u) > 2) {
            # removes the repeated element & the first element in the circuit
            path_back <- path_v_u[c(-1, -length(path_v_u))]
          }

          circuit <- c(path_u_v, path_back)

          if(sum(duplicated(circuit)) == 0) {
            loops_in_ss <<- c(loops_in_ss, list(circuit))
          }
        })
      }
    }
    is_duplicated <- duplicated(lapply(loops_in_ss, sort))
    unique_loops <- loops_in_ss[!is_duplicated]
  })

  geodetic_cycles <- delete.NULLs(geodetic_cycles)
  geodetic_cycles <- unlist(geodetic_cycles, recursive = FALSE)
  names_gc        <- paste0("L", 1:length(geodetic_cycles))
  names(geodetic_cycles) <- names_gc
  geodetic_cycles
}
