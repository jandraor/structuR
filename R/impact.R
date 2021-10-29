impact <- function(o, d, v_df) {

  o_dt <- subset(v_df, stock == o)[, "equation"]
  d_dt <- subset(v_df, stock == d)[, "equation"]
  pd   <- Deriv::Deriv(d_dt, o) # Partial derivative

  stringr::str_glue("{pd} * ({o_dt}) / ({d_dt})")
}

impacts_on <- function(d, path_df, v_df) {

  d_df <- subset(path_df, destination == d) # Destination data.frame

  d_list <- purrr::transpose(d_df)

  sapply(d_list, function(pathway) {
    o <- pathway$origin
    d <- pathway$destination

    impact(o, d, v_df)
  }) -> impacts

  d_df$impact <- impacts

  d_df
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

