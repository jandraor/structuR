calculate_loop_gains <- function(graph, sim_df = NULL, time_steps = NULL,
                                 method = "analytical", constants = NULL) {

  if(method == "analytical") {
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

    return(list(graph         = graph,
                loop_gains    = loop_gains))
  }

  if(method == "numerical") {
    SILS_pathways <- find_SILS(graph)

    loops <- lapply(SILS_pathways, function(pw) circuit <- c(pw, pw[1]))

    loops_id <- names(loops)

    loop_gains <- purrr::map_df(loops_id, function(loop_id, time_steps, loops) {
      loop <- loops[[loop_id]]

      # single timestep gain
      single_ts_gain <- purrr::map_df(time_steps, function(time_step, loop, graph, sim_df) {
        n_edges <- length(loop) - 1
        gains   <- vector(length = n_edges)

        for(i in seq_len(n_edges)) {
          u <- loop[i]
          v <- loop[i + 1]
          v_equation       <- igraph::V(graph)[name == v]$equation
          arg_names        <- names(igraph::neighbors(graph, v, mode = "in"))

          pos_u <- which(arg_names == u)
          arg_names <- c(u, arg_names[-pos_u])

          func_args        <- sim_df %>% dplyr::filter(time == time_step) %>%
            dplyr::select(arg_names) %>% unlist()

          names(func_args) <- arg_names

          func <- rlang::new_function(func_args, rlang::parse_expr(v_equation))
          gains[i] <- numDeriv::grad(func, func_args[u])
        }

        data.frame(time = time_step, gain = prod(gains))

      }, loop = loop, graph = graph, sim_df = sim_df)

      single_ts_gain %>% dplyr::mutate(loop_id = loop_id)


    }, time_steps = time_steps, loops = loops)
  }
}
