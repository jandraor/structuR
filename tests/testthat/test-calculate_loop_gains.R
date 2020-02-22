#===============================================================================
# LOTKA-VOLTERRA MODEL
#===============================================================================

sim_df_lv <- data.frame(
  time = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
           15L),
  x = c(10, 18.20206072, 29.72163983, 30.1535988, 13.11687131,
        5.011520952, 3.076868069, 2.998919142,
        4.035403201, 6.63185837, 12.07561293, 22.04031167,
        33.2393666, 25.55461732, 9.066448797),
  y = c(2, 2.093952198, 3.292712053, 7.129438376, 10.33222115,
        8.718380864, 6.153545716, 4.193394406,
        2.912137627, 2.168245724, 1.884539291, 2.217144418,
        4.111482932, 8.851490253, 10.37721981),
  Bx = c(10, 18.20206072, 29.72163983, 30.1535988, 13.11687131,
         5.011520952, 3.076868069, 2.998919142,
         4.035403201, 6.63185837, 12.07561293, 22.04031167,
         33.2393666, 25.55461732, 9.066448797),
  Dx = c(4, 7.622849011, 19.57296034, 42.9956449, 27.10528303,
         8.738469672, 3.786729665, 2.515130151,
         2.350329901, 2.875899711, 4.551393406, 9.773310798,
         27.3326177, 45.23928922, 18.81690642),
  By = c(0.8, 1.524569802, 3.914592067, 8.59912898, 5.421056605,
         1.747693934, 0.757345933, 0.50302603, 0.47006598,
         0.575179942, 0.910278681, 1.95466216, 5.466523539,
         9.047857845, 3.763381284),
  Dy = c(1, 1.046976099, 1.646356026, 3.564719188, 5.166110574,
         4.359190432, 3.076772858, 2.096697203,
         1.456068814, 1.084122862, 0.942269646, 1.108572209,
         2.055741466, 4.425745127, 5.188609907),
  c = c(0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04,
        0.04, 0.04, 0.04, 0.04, 0.04),
  d = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
        0.5, 0.5, 0.5),
  b = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
        0.2, 0.2, 0.2),
  a = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
)


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
                     equation = c("Bx-Dx", "By-Dy", "1*x", "0.2*x*y", "0.04*x*y", "0.5*y")
  )
)

gr_lotka_volterra <- igraph::graph_from_data_frame(lv_dfs$edges, directed = T,
                                                   vertices = lv_dfs$nodes)

#===============================================================================

actual_lg  <- calculate_loop_gains(gr_lotka_volterra,
                                   sim_df_lv, c(1, 5, 10), "numerical")
test_that("calculate_loop_gains returns the loop gains in numerical approx mode
          for L1", {
  # Testing Lotka-Volterra model

  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L1") %>%
    dplyr::pull(gain)
  expect_equal(gain, 1)
})

test_that("calculate_loop_gains returns the loop gains in numerical approx mode
          for L2", {
  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.4, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -2.06644, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L2") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.43365, tolerance = 1e-3)
})

test_that("calculate_loop_gains returns the loop gains in numerical approx mode
          for L3", {
  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.4, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.52467, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L3") %>%
    dplyr::pull(gain)
  expect_equal(gain, 0.26527, tolerance = 1e-3)
})

test_that("calculate_loop_gains returns the loop gains in numerical approx mode
          for L4", {
  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L4") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L4") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L4") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.5)
})

test_that("calculate_loop_gains returns the loop gains in numerical approx mode
          for L5", {
  gain <- actual_lg %>% dplyr::filter(time == 1, loop_id == "L5") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.16, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 5, loop_id == "L5") %>%
    dplyr::pull(gain)
  expect_equal(gain, -1.08421, tolerance = 1e-3)
  gain <- actual_lg %>% dplyr::filter(time == 10, loop_id == "L5") %>%
    dplyr::pull(gain)
  expect_equal(gain, -0.11504, tolerance = 1e-3)
})


