context("Find directed cycles")

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

test_that("the function returns the correct number of directed cycles", {
  directed_cycles <- find_directed_cycles(gr)
  n_loops <- length(directed_cycles)
  expect_equal(n_loops, 3)
})
