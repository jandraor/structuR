delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

# Based on Kampmann (1996)

find_directed_cycles <- function(graph) {
  nodes   <- names(igraph::V(graph))
  n_nodes <- igraph::gorder(graph)
  loops   <- NULL

  for(i in 1:n_nodes) {
    node <- nodes[[i]]
    neighbours <- igraph::neighbors(graph, node , mode = "out")

    #pathways per neighbour
    pathways_nb <- lapply(neighbours, function(neighbor) {
      igraph::all_simple_paths(graph, neighbor, node, mode = "out")
    })

    pathways_nb <- delete.NULLs(pathways_nb)

    if(length(pathways_nb) > 0) {

      pathway_list <- unlist(pathways_nb, recursive = FALSE, use.names = FALSE)

      discovered_loops <- lapply(pathway_list, function(pathway) {
        pathway %>% as.list() %>% unlist()
      })

      loops <- c(loops, discovered_loops)
    }

    graph <- igraph::delete_vertices(graph, node)
  }

  n_loops <- length(loops)
  names(loops) <- paste0("L", 1:n_loops)
  loops
}
