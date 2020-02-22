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

#===============================================================================
nf_dfs <- list(
  edges = data.frame(stringsAsFactors=FALSE,
                  from = c("cAY", "cEMP", "Y", "FS", "KI", "KD", "cLED", "cLU", "RCM",
                           "cP", "cPY", "cSED", "LED", "K", "EMP", "K", "ek",
                           "M", "em", "P", "AY", "EMP", "PTY", "SED", "ek", "ntr",
                           "PY", "apc", "Y", "AY", "U", "LU", "P", "U", "SED",
                           "rw", "DIV", "IV", "LED", "R", "KD", "DK", "K", "LU",
                           "EMP", "DEMP", "ntr", "Y", "em", "PT", "DEMP", "EMP",
                           "PT", "PT", "CGS", "CGT", "M", "PT", "TMS", "Y", "T",
                           "GT", "CDY", "PY", "C", "KI", "G", "FS", "DII", "A",
                           "LED", "A", "SED"),
                    to = c("AY", "EMP", "IV", "IV", "K", "K", "LED", "LU", "M", "P",
                           "PY", "SED", "DIV", "KD", "PTY", "PTY", "PTY", "R",
                           "R", "R", "R", "U", "Y", "Y", "apc", "apc", "C", "C",
                           "cAY", "cAY", "cLU", "cLU", "cP", "cP", "DEMP",
                           "DEMP", "DII", "DII", "DK", "DK", "KI", "KI", "KI", "PT",
                           "PT", "PT", "T", "T", "TMS", "TMS", "cEMP", "cEMP",
                           "CGS", "CGT", "G", "GT", "RCM", "RCM", "RCM", "CDY",
                           "CDY", "CDY", "cPY", "cPY", "FS", "FS", "FS", "A", "A",
                           "cLED", "cLED", "cSED", "cSED"),
                  type = c("flow", "flow", "flow", "flow", "flow", "flow", "flow",
                           "flow", "flow", "flow", "flow", "flow", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link")
          ),
  nodes = data.frame(stringsAsFactors=FALSE,
                  name = c("AY", "EMP", "IV", "K", "LED", "LU", "M", "P", "PY", "SED",
                           "DIV", "ek", "em", "KD", "ntr", "PTY", "R", "rw",
                           "U", "Y", "apc", "C", "cAY", "cLU", "cP", "DEMP", "DII",
                           "DK", "KI", "PT", "T", "TMS", "cEMP", "CGS", "CGT",
                           "G", "GT", "RCM", "CDY", "cPY", "FS", "A", "cLED",
                           "cSED"),
                  type = c("stock", "stock", "stock", "stock", "stock", "stock", "stock",
                           "stock", "stock", "stock", "variable", "variable",
                           "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable",
                           "variable")
          )
)

gr_NF <- graph_from_data_frame(nf_dfs$edges, directed = T,
                                           vertices = nf_dfs$nodes)
#===============================================================================

test_that("the function returns the correct number of geodetic cycles", {
  geodetic_cycles <- find_geodetic_cycles(gr)
  n_geo_cycles    <- length(geodetic_cycles)
  expect_equal(n_geo_cycles, 3)

  geodetic_cycles <- find_geodetic_cycles(gr_lotka_volterra)
  n_geo_cycles    <- length(geodetic_cycles)
  expect_equal(n_geo_cycles, 5)

  geodetic_cycles <- find_geodetic_cycles(gr_NF)
  n_geo_cycles    <- length(geodetic_cycles)
  expect_equal(n_geo_cycles, 58)
})
