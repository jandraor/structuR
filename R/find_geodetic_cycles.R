delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

#'  @export
find_geodetic_cycles <- function(graph) {
  strong_components   <- igraph::components(graph, mode = "strong")

  members <- igraph::membership(igraph::clusters(gr, mode = "strong"))

  strong_subgraphs <- lapply(unique(members), gr = gr,
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

    loops_in_ss <- lapply(loop_lengths, function(loop_length) {
      pairs <- which(loop_matrix == loop_length, arr.ind = TRUE)


      loops_found <- NULL

      for(i in seq_along(length(pairs))) {
        u <- nodes[pairs[i, 1]]
        v <- nodes[pairs[i, 2]]
        path_u_v <- names(igraph::all_simple_paths(graph, from = u, to = v)[[1]])
        path_v_u <- names(igraph::all_simple_paths(graph, from = v, to = u)[[1]])

        path_back <- NULL
        if(length(path_v_u) > 2) {
          path_back <- path_v_u[c(-1, -length(path_v_u))]
        }

        circuit <- c(path_u_v, path_back)

        if(sum(duplicated(circuit)) > 0) {
          next
        }

        loops_found <- c(loops_found, circuit)

      }
      loops_found
    })
    loops_in_ss <- delete.NULLs(loops_in_ss)

  })
  geodetic_cycles <- delete.NULLs(geodetic_cycles)

  unlist(geodetic_cycles, recursive = FALSE)
}
