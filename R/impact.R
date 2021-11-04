#' Construct analytical impact equations for a given stock (level)
#'
#' @param stk A string that indicates the stock's name
#' @param inputs A list of three elements (flows, pathways, velocities). One can
#' obtain this list automatically from the function \code{sd_impact_inputs} from
#' the package \code{readsdr}.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#'  flows <- data.frame(
#'    stock    = c("x", "y", "y", "z", "z"),
#'    flow     = c("f1", "f1", "f2", "f2", "f3"),
#'    sign     = c("-", "+", "-", "+", "-"),
#'    equation = c("R*x*z", "R*x*z", "a*y", "a*y", "y"))
#'
#'  pathways <- data.frame(
#'  from    = c("z", "x", "z", "x", "y", "y", "z"),
#'  to      = c("x", "x", "y", "y", "y", "z", "z"),
#'  through = c("f1", "f1", "f1", "f1", "f2", "f2", "f3"))
#'
#'  velocities <- data.frame(
#'    stock    = c("x", "y", "z"),
#'    equation = c(
#'      "-R * x * z",
#'      " R * x * z - a * y",
#'      "a * y - z"))
#'
#'  inputs <- list(flows      = flows
#'                 pathways   = pathways,
#'                 velocities = velocities)
#'
#'  struc_impacts_on("x", inputs)
struc_impacts_on <- function(stk, inputs) {

  v_df <- inputs$velocities

  flows_df           <- subset(inputs$flows, stock == stk)
  flows_df$signed_eq <- stringr::str_glue("{flows_df$sign}({flows_df$equation})")

  pathways_df <- subset(inputs$pathways, to == stk)

  pathways_df <- merge(pathways_df, flows_df[, c("flow", "signed_eq")],
                       by.x = "through", by.y = "flow", all.y = FALSE)

  pw_list <- purrr::transpose(pathways_df)

  sapply(pw_list, function(pathway) {
    eq   <- pathway$signed_eq
    from <- pathway$from
    to   <- pathway$to

    impact(eq, from, to, v_df)
  }) -> impacts

  pathways_df$impact <- impacts

  pathways_df[, c("from", "to", "through", "impact")]
}

#' Evaluate impact equations numerically
#'
#' @param impact_df Data frame returned by the function \code{struc_impact_on}
#' @param sim_df Data frame with the simulation results
#'
#' @return
#' @export
#'
#' @examples
#' I_z_x <- "-(R * x) * (a * y - z) / (-R * x * z)"
#' I_x_x <- "-(R * z) * (-R * x * z) / (-R * x * z)"
#'
#' impact_df <- data.frame(from    = c("z", "x"),
#'                         to      = c("x", "x"),
#'                         through = c("f1", "f1"),
#'                         impact  = c(I_z_x, I_x_x))
#'
#' x0  <- 0.974182771
#' x10 <- 0.466656479
#' y0  <- 0.009831036
#' y10 <- 0.078797261
#' z0  <- 0.007933189
#' z10 <- 0.078490487
#'
#' sim_df <- data.frame(
#'   time = c(0L, 10L),
#'   x = c(x0, x10),
#'   y = c(y0, y10),
#'   z = c(z0, z10),
#'   R = c(2L, 2L),
#'   a = c(1L, 1L))
#' struc_eval_impact(impact_df, sim_df)
struc_eval_impact <- function(impact_df, sim_df) {

  times <- sim_df$time

  impact_list <- purrr::transpose(impact_df)

  purrr::map_dfc(impact_list, function(row) {

    col_name  <- stringr::str_glue("I_{row$from}_{row$to}")
    vals      <- with(sim_df, eval(parse(text = row$impact)))
    df        <- data.frame(vals)
    names(df) <- col_name
    df
  }) -> impact_cols

  cbind(sim_df["time"], impact_cols)

}

impact <- function(eq, from, to, v_df) {

  from_dt <- subset(v_df, stock == from)[, "equation"]
  to_dt   <- subset(v_df, stock == to)[, "equation"]
  pd      <- Deriv::Deriv(eq, from) # Partial derivative

  stringr::str_glue("{pd} * ({from_dt}) / ({to_dt})")
}

