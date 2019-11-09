calculate_loop_gains <- function(graph, sim_df = NULL, time_steps = NULL,
                                 method = "analytical") {
  edges   <- igraph::E(graph)
  n_edges <- igraph::gsize(graph)

  edge_gains <- vector(length = n_edges) # Empty vector of 'n_edges' size

  for(i in 1:n_edges) {
    edge               <- edges[[i]]
    edge_head          <- igraph::head_of(graph, edge)
    edge_tail          <- igraph::tail_of(graph, edge)
    head_equation_text <- igraph::V(graph)[edge_head]$equation
    with_respect_to    <- names(edge_tail)
    equation           <- parse(text = head_equation_text)
    derivative         <- D(equation, with_respect_to)
    edge_gains[i]      <- deparse(derivative)
  }

  igraph::E(graph)$edge_gain <- edge_gains

  SILS_pathways <- find_SILS(graph)
  loop_names    <- names(SILS_pathways)

  loop_gains <- purrr::map_df(SILS_pathways, function(pathway, graph){
    loop         <- c(pathway, pathway[1]) # closes the loop
    i_length     <- length(loop) - 1
    gains_vector <- vector()

    for(i in 1:i_length) {
      node_tail    <- loop[i]
      node_head    <- loop[i + 1]
      edge_gain    <- igraph::E(graph, path = c(node_tail, node_head))$edge_gain
      edge_gain    <- paste("( ", edge_gain, " )")
      gains_vector <- c(gains_vector, edge_gain)
    }

    gain_expression <- paste(gains_vector, collapse = " * ")

    loop_path       <- paste(as.character(pathway), collapse = " -> ")

    data.frame(loop_path = loop_path,
               gain = gain_expression, stringsAsFactors = F)

  }, graph = graph)

  loop_gains$loop_id <- loop_names
  loop_gains         <- loop_gains[, c(3, 1, 2)]

  list(graph         = graph,
       loop_gains    = loop_gains)
}
