approx_edge_gain <- function(graph, v, u, row_df) {
  v_equation       <- igraph::V(graph)[name == v]$equation
  arg_names        <- names(igraph::neighbors(graph, v, mode = "in"))

  pos_u <- which(arg_names == u)
  arg_names <- c(u, arg_names[-pos_u])

  func_args        <- row_df %>% dplyr::select(arg_names) %>% unlist()

  names(func_args) <- arg_names

  func <- rlang::new_function(func_args, rlang::parse_expr(v_equation))
  numDeriv::grad(func, func_args[u])
}
