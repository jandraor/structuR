#===============================================================================
# LOTKA-VOLTERRA MODEL
#===============================================================================
row_df <- data.frame(
  time = c(5L),
  x = c(13.11687131),
  y = c(10.33222115),
  Bx = c(13.11687131),
  Dx = c(27.10528303),
  By = c(5.421056605),
  Dy = c(5.166110574),
  c = c(0.04),
  d = c(0.5),
  b = c(0.2),
  a = c(1L)
)

lv_dfs <- list(
  edges = data.frame(stringsAsFactors=FALSE,
                     from = c("Bx", "Dx", "By", "Dy", "x", "x", "y", "x", "y", "y"),
                     to = c("x", "x", "y", "y", "Bx", "Dx", "Dx", "By", "By", "Dy"),
                     type = c("flow", "flow", "flow", "flow", "info_link", "info_link",
                              "info_link", "info_link", "info_link", "info_link")
  ),
  nodes = data.frame(stringsAsFactors=FALSE,
                     name = c("x", "y", "Bx", "Dx", "By", "Dy"),
                     type = c("stock", "stock", "variable", "variable", "variable",
                              "variable"),
                     equation = c("Bx-Dx", "By-Dy", "1*x", "0.2*x*y", "0.04*x*y", "0.5*y")
  )
)

gr_lotka_volterra <- igraph::graph_from_data_frame(lv_dfs$edges, directed = T,
                                                   vertices = lv_dfs$nodes)

#==============================================================================


test_that("approx_edge_gain() returns the expected gain", {
  actual_gain <- approx_edge_gain(gr_lotka_volterra, "By", "x", row_df)
  expected_gain <- 10.33222115 * 0.04
  expect_equal(actual_gain, expected_gain)
  #, tolerance = 1e-5
})
