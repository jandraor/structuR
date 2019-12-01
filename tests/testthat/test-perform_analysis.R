context("Perform analysis")

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


test_that("perform_analysis() returns the expected eigenvalues", {
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
    expect_equal(EV1$real_value, expected_ev$EV1_real, tol = 1e-5)
    expect_equal(EV1$imaginary_value, expected_ev$EV1_img, tol = 1e-5)
    EV2 <- eigenvalues_df[eigenvalues_df$eigenvalue == "EV2", ]
    expect_equal(EV2$real_value, expected_ev$EV2_real, tol = 1e-5)
    expect_equal(EV2$imaginary_value, expected_ev$EV2_img, tol = 1e-5)
  }

})

test_that("perform_analysis() returns the expected loop analysis", {
  expected_las <- list(
    list(EV1_L1_Re = 0.5, EV1_L1_Im = -0.90370, EV1_L1_abs = 1.03280,
         EV1_L2_Re = -0.2, EV1_L2_Im = 0.36148, EV1_L2_abs = 0.41312,
         EV1_L3_Re = 0, EV1_L3_Im = 0.41312, EV1_L3_abs = 0.41312,
         EV1_L4_Re = 0.2, EV1_L4_Im = 0.36148, EV1_L4_abs = 0.41312,
         EV1_L5_Re = -0.25, EV1_L5_Im = -0.45185, EV1_L5_abs = 0.51640,
         EV2_L1_Re = 0.5, EV2_L1_Im = 0.90370, EV2_L1_abs = 1.03280,
         EV2_L2_Re = -0.2, EV2_L2_Im = -0.36148, EV2_L2_abs = 0.41312,
         EV2_L3_Re = 0, EV2_L3_Im = -0.41312, EV2_L3_abs = 0.41312,
         EV2_L4_Re = 0.2, EV2_L4_Im = -0.36148, EV2_L4_abs = 0.41312,
         EV2_L5_Re = -0.25, EV2_L5_Im = 0.45185, EV2_L5_abs = 0.51640),
    list(EV1_L1_Re = 0.5, EV1_L1_Im = 0.30757, EV1_L1_abs = 0.58703,
         EV1_L2_Re = -1.03322, EV1_L2_Im = -0.63557, EV1_L2_abs = 1.21305,
         EV1_L3_Re = 0, EV1_L3_Im = 0.61124, EV1_L3_abs = 0.61124,
         EV1_L4_Re = 0.26234, EV1_L4_Im = -0.16137, EV1_L4_abs = 0.30800,
         EV1_L5_Re = -0.25, EV1_L5_Im = 0.15378, EV1_L5_abs = 0.29351,
         EV2_L1_Re = 0.5, EV2_L1_Im = -0.30757, EV2_L1_abs = 0.58703,
         EV2_L2_Re = -1.03322, EV2_L2_Im = 0.63557, EV2_L2_abs = 1.21305,
         EV2_L3_Re = 0, EV2_L3_Im = -0.61124, EV2_L3_abs = 0.61124,
         EV2_L4_Re = 0.26234, EV2_L4_Im = 0.16137, EV2_L4_abs = 0.30800,
         EV2_L5_Re = -0.25, EV2_L5_Im = -0.15378, EV2_L5_abs = 0.29351),
    list(EV1_L1_Re = 1.43996, EV1_L1_Im = 0, EV1_L1_abs = 1.43996,
         EV1_L2_Re = -0.62444, EV1_L2_Im = 0, EV1_L2_abs = 0.62444,
         EV1_L3_Re = -0.26996, EV1_L3_Im = 0, EV1_L3_abs = 0.26996,
         EV1_L4_Re = -0.11671, EV1_L4_Im = 0, EV1_L4_abs = 0.11671,
         EV1_L5_Re = 0.21998, EV1_L5_Im = 0, EV1_L5_abs = 0.21998,
         EV2_L1_Re = -0.43996, EV2_L1_Im = 0, EV2_L1_abs = 0.43996,
         EV2_L2_Re = 0.19079, EV2_L2_Im = 0, EV2_L2_abs = 0.19079,
         EV2_L3_Re = 0.26996, EV2_L3_Im = 0, EV2_L3_abs = 0.26996,
         EV2_L4_Re = 0.38199, EV2_L4_Im = 0, EV2_L4_abs = 0.38199,
         EV2_L5_Re = -0.71998, EV2_L5_Im = 0, EV2_L5_abs = 0.71998)
  )

  for(i in seq(1:length(expected_las))) {
    output_pa <- perform_analysis(
      Ams[[i]], Bms[[i]], Cms[[i]], Dms[[i]], gr_lotka_volterra, sim_df_lv[i, ],
      inv_cm, n_levels, "numerical")
    loop_analysis_df <- output_pa$loop_analysis
    # Expected loop analysis
    expected_la <- expected_las[[i]]

    EV1     <- loop_analysis_df[loop_analysis_df$eigenvalue_id == "EV1", ]
    loops   <- paste0("L", 1:5)

    for(loop in loops) {
      EV_L  <- EV1[EV1$loop_id == loop, ]
      expect_equal(EV_L$loop_influence_Re,
                   expected_la[[paste0("EV1_", loop,"_Re")]], tol = 1e-5)
      expect_equal(EV_L$loop_influence_Im,
                   expected_la[[paste0("EV1_", loop,"_Im")]], tol = 1e-5)
      expect_equal(EV_L$loop_influence_abs,
                   expected_la[[paste0("EV1_", loop,"_abs")]], tol = 1e-5)
    }

    EV2     <- loop_analysis_df[loop_analysis_df$eigenvalue_id == "EV2", ]

    for(loop in loops) {
      EV_L  <- EV2[EV2$loop_id == loop, ]
      expect_equal(EV_L$loop_influence_Re,
                   expected_la[[paste0("EV2_", loop,"_Re")]], tol = 1e-5)
      expect_equal(EV_L$loop_influence_Im,
                   expected_la[[paste0("EV2_", loop,"_Im")]], tol = 1e-5)
      expect_equal(EV_L$loop_influence_abs,
                   expected_la[[paste0("EV2_", loop,"_abs")]], tol = 1e-5)
    }
  }
})
