impact <- function(eq, from, to, v_df) {

  from_dt <- subset(v_df, stock == from)[, "equation"]
  to_dt   <- subset(v_df, stock == to)[, "equation"]
  pd      <- Deriv::Deriv(eq, from) # Partial derivative

  stringr::str_glue("{pd} * ({from_dt}) / ({to_dt})")
}

impacts_on <- function(stk, inputs) {

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

evaluate_impact <- function(impact_df, sim_df) {

  times <- sim_df$time

  impact_list <- purrr::transpose(impact_df)

  purrr::map_dfc(impact_list, function(row) {

    col_name  <- stringr::str_glue("I_{row$origin}_{row$destination}")
    vals      <- with(sim_df, eval(parse(text = row$impact)))
    df        <- data.frame(vals)
    names(df) <- col_name
    df
  }) -> impact_cols

  cbind(sim_df["time"], impact_cols)

}

