#' Run Loop Eigenvalue Elasticity Analysis
#'
#' @param times A numeric vector that indicates in which time steps the
#'   procedure is applied.
#' @param graph An igraph object representation of a System Dynamics model.
#' @param sim_df A dataframe with the simulation results of a System Dynamics
#'   model. This dataframe must contain the column 'time'.
#' @param method A string indicating the method for estimating edge gains. It
#'   can be either 'analytical' or 'numerical'.
#' @param graph_funs A list of graphical functions.
#'
#' @return
#' @export
#'
#' @examples
run_LEEA <- function (times, graph, sim_df, method = "analytical",
                      graph_funs = NULL) {

  sim_df <- sim_df[sim_df$time %in% times, ]

  if(method == "analytical") {

    if(!is.null(graph_funs)) {
      stop("Graphical functions cannot be solved analytically. Please change the method to 'numerical'",
           .call = TRUE)
    }

    return(LEEA_analytical(graph, sim_df))
  }

  if(method == "numerical") LEEA_numerical(graph, sim_df, graph_funs)
}

