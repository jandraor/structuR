context("Run LEEA")

#===============================================================================
# SIR MODEL
#===============================================================================

sim_df <- data.frame(
  time = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L,
           16L, 17L, 18L, 19L, 20L),
  Infected = c(10, 38.06228003, 128.7492011, 309.681701, 418.5310934,
               359.65264, 254.1038897, 166.9712162,
               106.5035486, 66.99632459, 41.84268188, 26.02951035,
               16.15549088, 10.01352474, 6.201550627,
               3.838827826, 2.375556653, 1.469776466,
               0.909258506, 0.562461397, 0.347919755),
  Recovered = c(0, 9.694049501, 44.75007464, 146.5510672, 332.834434,
                533.7723592, 690.4103882, 797.2755891,
                866.5289667, 910.4237301, 937.9470627,
                955.1065822, 965.7704306, 972.3851304, 976.4836034,
                979.0213097, 980.5919697, 981.5638519,
                982.1651333, 982.5370969, 982.7671868),
  Susceptible = c(990, 952.2436705, 826.5007243, 543.7672318, 248.6344726,
                  106.5750009, 55.48572206, 35.75319469,
                  26.96748463, 22.57994526, 20.21025542,
                  18.86390745, 18.07407851, 17.60134487, 17.31484599,
                  17.13986251, 17.03247365, 16.96637162,
                  16.92560816, 16.90044166, 16.88489343),
  ce = c(20, 76.12456006, 257.4984022, 619.3634019, 837.0621868,
         719.3052799, 508.2077795, 333.9424325,
         213.0070973, 133.9926492, 83.68536377,
         52.0590207, 32.31098176, 20.02704948, 12.40310125,
         7.677655651, 4.751113306, 2.939552933,
         1.818517012, 1.124922794, 0.69583951),
  cpi = c(80, 304.4982402, 1029.993609, 2477.453608, 3348.248747,
          2877.22112, 2032.831118, 1335.76973,
          852.028389, 535.9705967, 334.7414551, 208.2360828,
          129.2439271, 80.1081979, 49.61240502,
          30.71062261, 19.00445323, 11.75821173, 7.274068049,
          4.499691176, 2.78335804),
  IR = c(19.8, 72.48913048, 212.8226159, 336.7895226, 208.1225153,
         76.65996084, 28.1982756, 11.9395088,
         5.744265621, 3.025546684, 1.691302577, 0.982036548,
         0.583991221, 0.352503004, 0.214757788,
         0.131593962, 0.080923212, 0.049873547,
         0.030779506, 0.019011692, 0.011749176),
  probability = c(0.99, 0.95224367, 0.826500724, 0.543767232, 0.248634473,
                  0.106575001, 0.055485722, 0.035753195,
                  0.026967485, 0.022579945, 0.020210255,
                  0.018863907, 0.018074079, 0.017601345, 0.017314846,
                  0.017139863, 0.017032474, 0.016966372,
                  0.016925608, 0.016900442, 0.016884893),
  RR = c(5, 19.03114001, 64.37460054, 154.8408505, 209.2655467,
         179.82632, 127.0519449, 83.48560811,
         53.25177431, 33.4981623, 20.92134094, 13.01475518,
         8.077745441, 5.006762369, 3.100775314,
         1.919413913, 1.187778327, 0.734888233, 0.454629253,
         0.281230698, 0.173959878),
  cr = c(8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L,
         8L, 8L, 8L, 8L, 8L, 8L, 8L),
  infectivity = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                  0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                  0.25, 0.25, 0.25, 0.25, 0.25),
  population = c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,
                 1000, 1000, 1000, 1000, 1000, 1000, 1000,
                 1000, 1000, 1000, 1000, 1000),
  recoveryDelay = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                    2L, 2L, 2L, 2L, 2L, 2L, 2L))

edges <- data.frame(stringsAsFactors=FALSE,
        from = c("IR", "RR", "RR", "IR", "cpi", "Infected", "ce",
                 "probability", "Susceptible", "Infected"),
          to = c("Infected", "Infected", "Recovered", "Susceptible", "ce",
                 "cpi", "IR", "IR", "probability", "RR"),
        type = c("flow", "flow", "flow", "flow", "info_link", "info_link",
                 "info_link", "info_link", "info_link", "info_link")
)


nodes <- data.frame(stringsAsFactors=FALSE,
        name = c("Infected", "Recovered", "Susceptible", "ce", "cpi", "IR",
                 "probability", "RR"),
        type = c("stock", "stock", "stock", "variable", "variable", "variable",
                 "variable", "variable"),
    equation = c("IR-RR", "RR", "-IR", "cpi*0.25", "Infected*8",
                 "ce*probability", "Susceptible/1000", "Infected/2")
)

graph <- igraph::graph_from_data_frame(edges, directed = T,
                            vertices = nodes)

#===============================================================================
# LOTKA-VOLTERRA MODEL
#===============================================================================

sim_df_lv <- data.frame(
  time = c(1L, 5L, 10L),
  x = c(10, 13.11687131, 6.63185837),
  y = c(2, 10.33222115, 2.168245724),
  Bx = c(10, 13.11687131, 6.63185837),
  Dx = c(4, 27.10528303, 2.875899711),
  By = c(0.8, 5.421056605, 0.575179942),
  Dy = c(1, 5.166110574, 1.084122862),
  c = c(0.04, 0.04, 0.04),
  d = c(0.5, 0.5, 0.5),
  b = c(0.2, 0.2, 0.2),
  a = c(1L, 1L, 1L)
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

test_that("the function returns a list in analytical mode", {
  output_LEEA <- run_LEEA(graph, sim_df, method = "analytical")
  expect_is(output_LEEA, "list")
})

test_that("the function returns a list in numerical mode", {
  output_LEEA <- run_LEEA(graph, sim_df, method = "numerical")
  expect_is(output_LEEA, "list")
})

test_that("the function returns the expected properties in analytical mode", {
  output_LEEA <- run_LEEA(graph, sim_df, method = "analytical")
  expected_properties <- names(output_LEEA)
  expect_equal("loop_gains" %in% expected_properties, TRUE)
  expect_equal("eigenvalues" %in% expected_properties, TRUE)
  expect_equal("loop_analysis" %in% expected_properties, TRUE)
  expect_equal("gains_matrices" %in% expected_properties, TRUE)
  expect_equal("graph" %in% expected_properties, TRUE)
})

test_that("the function returns the expected properties in numerical mode", {
  output_LEEA <- run_LEEA(graph, sim_df, method = "numerical")
  expected_properties <- names(output_LEEA)
  expect_equal("loop_gains" %in% expected_properties, TRUE)
  expect_equal("eigenvalues" %in% expected_properties, TRUE)
  expect_equal("loop_analysis" %in% expected_properties, TRUE)
  expect_equal("gains_matrices" %in% expected_properties, TRUE)
  expect_equal("graph" %in% expected_properties, TRUE)
})

test_that("the property eigenvalues returned by run_LEEA() in numerical mode is a data frame ", {
  output_LEEA <- run_LEEA(graph, sim_df, method = "numerical")
  expect_is(output_LEEA$eigenvalues, "data.frame")
})

test_that("the property loop_analysis returned by run_LEEA() in numerical mode is a data frame ", {
  output_LEEA <- run_LEEA(graph, sim_df, method = "numerical")
  expect_is(output_LEEA$loop_analysis, "data.frame")
})

test_that("the function returns the loop gains in analytical mode", {
  # Testing SIR model

  output_LEEA <- run_LEEA(graph, sim_df, method = "analytical")
  actual_lg   <- output_LEEA$loop_gains

  gain <- actual_lg %>% dplyr::filter(time == 0, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)

  gain <- actual_lg %>% dplyr::filter(time == 0, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.02, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.71931, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.08369, tolerance = 1e-3)

  gain <- actual_lg %>% dplyr::filter(time == 0, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1.98, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.21315, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.04042, tolerance = 1e-3)

  # Testing Lotka-Volterra model

  output_LEEA <- run_LEEA(gr_lotka_volterra, sim_df_lv, method = "analytical")
  actual_lg   <- output_LEEA$loop_gains

  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1)

  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.4, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -2.06644, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.43365, tolerance = 1e-3)

  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.16, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, -1.08421, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.11504, tolerance = 1e-3)

  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L4") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.4, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L4") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.52467, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L4") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.26527, tolerance = 1e-3)

  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L5") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L5") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L5") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
})
