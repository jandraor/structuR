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

levels    <-  names(igraph::V(gr_lotka_volterra)[[type == "stock"]])
variables <- names(igraph::V(gr_lotka_volterra)[[type == "variable"]])
n_levels  <- length(levels)
n_vars    <- length(variables)

Am  <- matrix(0, nrow = n_levels, ncol = n_levels)
colnames(Am) <- levels
rownames(Am) <- levels
Ams <- list(Am, Am, Am)

Bm           <- matrix(c(1, -1, 0, 0, 0, 0, 1, -1), nrow = n_levels,
                       ncol = n_vars, byrow = T)
colnames(Bm) <- variables
rownames(Bm) <- levels
Bms          <- list(Bm, Bm, Bm)

Cm1 <- matrix(c(1, 0, 0.4, 2.0, 0.08, 0.4, 0, 0.5), nrow = n_vars,
              ncol = n_levels, byrow = T, dimnames = list(variables, levels))

Cm2 <- matrix(c(1, 0, 2.0664442, 2.6233743, 0.4132888, 0.5246749, 0, 0.5),
              nrow = n_vars, ncol = n_levels, byrow = T,
              dimnames = list(variables, levels))

Cm3 <- matrix(c(1, 0, 0.43364914, 1.3263717, 0.08672983, 0.2652743, 0, 0.5),
              nrow = n_vars, ncol = n_levels, byrow = T,
              dimnames = list(variables, levels))

Cms          <- list(Cm1, Cm2, Cm3)

Dm  <- matrix(0, nrow = n_vars, ncol = n_vars,
              dimnames = list(variables, variables))

Dms          <- list(Dm, Dm, Dm)

values_inv_cm <- c(0.5, 0, 0, 0, 0.5, 0, 0, 0, 0, 0,
            0, 0.416666666666666, -0.0833333333333334, 0, 0, 0.583333333333333, -0.166666666666667, -0.166666666666667, 0.0833333333333332, 0,
            0, -0.0833333333333332, 0.416666666666667, 0, 0, 0.0833333333333333, -0.166666666666667, -0.166666666666667, 0.583333333333333, 0,
            0, 0.166666666666667, 0.166666666666667, 0, 0, -0.166666666666667, 0.333333333333333, 0.333333333333333, -0.166666666666667, 0,
            0, 2.77555756156289e-17, 2.77555756156289e-17, 0.5, 0, -2.77555756156289e-17, 3.92523114670944e-17, 3.92523114670944e-17, -2.77555756156289e-17, 0.5)

inv_cm <- matrix(
  values_inv_cm, byrow = T, ncol = 10, nrow =5,
  dimnames = list(c("L1", "L2", "L4", "L3", "L5"),
                  c("Bx|x", "Dx|x", "By|y", "Dy|y", "x|Bx", "x|Dx", "y|Dx", "x|By", "y|By", "y|Dy")))


test_that("perform_analysis() returns the correct eigenvalues", {
  expected_evs <- list(
    list(EV1_real = 0.25, EV1_img = 0.19365,
         EV2_real = 0.25, EV2_img = -0.19365),
    list(EV1_real = -0.52088, EV1_img = 0.88689,
         EV2_real = -0.52088, EV2_img = -0.88689),
    list(EV1_real = 0.37887, EV1_img = 0,
         EV2_real = -0.04725, EV2_img = 0)
    )

  for(i in seq(1:length(expected_evs))) {
    output_pa <- perform_analysis(
      Ams[[i]], Bms[[i]], Cms[[i]], Dms[[i]], gr_lotka_volterra, sim_df_lv[i, ],
      inv_cm, n_levels, "numerical")
    eigenvalues_df <- output_pa$eigenvalues
    expected_ev <- expected_evs[[i]]
    EV1 <- eigenvalues_df[eigenvalues_df$eigenvalue == "EV1", ]
    expect_equal(EV1$real_value, expected_ev$EV1_real, tol = 1e5)
    expect_equal(EV1$imaginary_value, expected_ev$EV1_img, tol = 1e5)
    EV2 <- eigenvalues_df[eigenvalues_df$eigenvalue == "EV2", ]
    expect_equal(EV2$real_value, expected_ev$EV2_real, tol = 1e5)
    expect_equal(EV2$imaginary_value, expected_ev$EV2_img, tol = 1e5)
  }

})
