context("Find geodetic cycles")

library(igraph)

graph_dfs <- list(
  edges = data.frame(stringsAsFactors = FALSE,
                     from = c("IR", "RR", "RR", "IR", "Susceptible", "Infected",
                              "cpi", "ce", "probability", "Infected"),
                     to = c("Infected", "Infected", "Recovered", "Susceptible",
                            "probability", "cpi", "ce", "IR", "IR", "RR")
  ),
  nodes = data.frame(stringsAsFactors = FALSE,
                     name = c("Infected", "Recovered", "Susceptible",
                              "probability", "cpi", "ce", "IR", "RR"),
                     type = c("stock", "stock", "stock", "variable", "variable",
                              "variable", "variable", "variable")
  )
)

gr <- graph_from_data_frame(graph_dfs$edges, directed = T,
                            vertices = graph_dfs$nodes)

lv_dfs <- list(
  edges = data.frame(stringsAsFactors=FALSE,
                   from = c("Bx", "Dx", "By", "Dy", "x", "x", "y", "x", "y", "y"),
                     to = c("x", "x", "y", "y", "Bx", "Dx", "Dx", "By", "By", "Dy"),
                   type = c("flow", "flow", "flow", "flow", "info_link", "info_link",
                            "info_link", "info_link", "info_link",
                            "info_link")),
  nodes = data.frame(stringsAsFactors=FALSE,
                  name = c("x", "y", "Bx", "Dx", "By", "Dy"),
                  type = c("stock", "stock", "variable", "variable", "variable",
                           "variable"),
              equation = c("Bx-Dx", "By-Dy", "a*x", "b*x*y", "c*x*y", "d*y")
          )
)

gr_lotka_volterra <- graph_from_data_frame(lv_dfs$edges, directed = T,
                                           vertices = lv_dfs$nodes)

test_that("the function returns the correct number of geodetic cycles", {
  geodetic_cycles <- find_geodetic_cycles(gr)
  n_geo_cycles    <- length(geodetic_cycles)
  expect_equal(n_geo_cycles, 3)

  geodetic_cycles <- find_geodetic_cycles(gr_lotka_volterra)
  n_geo_cycles    <- length(geodetic_cycles)
  expect_equal(n_geo_cycles, 5)
})
