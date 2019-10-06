calculate_weights <- function(comp, stocks) {

  weights_list <- list()
  theta_list   <- list()

  mode_index <- vector()

  skip <- 0

  for(i in 1:nrow(comp)){

    if(skip == 1) {
      skip <- 0
      next
    }

    behaviourMode <- comp[i, ]

    residual <- comp[i, ] - Conj(comp[i + 1, ])

    if(i < nrow(comp) && dplyr::near(sum(residual), 0)) {

      weights      <- 2 * sqrt(Re(behaviourMode) ^ 2 + Im(behaviourMode) ^ 2 )
      weights_list <- c(weights_list, list(weights))

      theta      <- Arg(behaviourMode) + pi / 2

      theta      <- sapply(1:length(theta), function(index){
        if(Im(behaviourMode[index]) >= 0 && Re(behaviourMode[index]) < 0 ) {
          theta[index] - 2 * pi
        } else {
          theta[index]
        }
      })

      theta_list <- c(theta_list, list(theta))

      ct         <- stocks - 2 * Re(behaviourMode)
      mode_index <- c(mode_index, i)

      skip       <- 1
      theta_list <- c(theta_list, list(NA))

    } else {
      weights <- Re(comp[i, ])
      theta   <- NA
      ct      <- stocks - 2 * Re(behaviourMode)
      mode_index <- c(mode_index, i)
    }

  }
  list(weights = weights_list,
       thetas = theta_list,
       constants = ct,
       mode_index = mode_index)
}


run_DDWA <- function(time_DDWA, sim_df, gains_matrices, mdl) {
  row <- dplyr::filter(sim_df, time == time_DDWA)

  vars_names         <- map_chr(mdl$description$variables, "name")
  vars_expr          <- lapply(vars_names, rlang::parse_expr)
  vars_values        <- sapply(vars_expr, function(x) with(row, eval(x)))
  names(vars_values) <- vars_names
  n_vars             <- length(vars_names)

  stock_names         <- map_chr(mdl$description$levels, "name")
  stock_expr          <- lapply(stock_names, rlang::parse_expr)
  stock_values        <- sapply(stock_expr, function(x) with(row, eval(x)))
  names(stock_values) <- stock_names

  Am       <- gains_matrices$A_matrix
  Bm       <- evaluate_matrix(gains_matrices$B_matrix, row)
  Cm       <- evaluate_matrix(gains_matrices$C_matrix, row)
  Dm       <- evaluate_matrix(gains_matrices$D_matrix, row)
  Um       <- solve(diag(nrow(Dm)) - Dm)
  Jacobian <- Am + Bm %*% Um %*% Cm

  db          <- ((diag(n_vars) - Dm) %*% vars_values) - (Cm %*% stock_values)
  nrt0        <- Jacobian %*% stock_values + Bm %*% Um %*% db

  eigensystem  <- eigen(Jacobian)
  eigenvalues  <- eigensystem$values
  eigenvectors <- t(eigensystem$vectors)

  #real part of behaviour modes
  real_part_bm        <- Re(eigenvalues)

  #imaginary part of behaviour modes
  img_part_bm         <- Im(eigenvalues)

  behaviour_modes_df  <- data.frame(stringsAsFactors = FALSE,
                                    behaviour_mode = 1:length(eigenvalues),
                                    real_part = real_part_bm,
                                    img_part = img_part_bm ) %>%
    filter(real_part !=0 & img_part != 0)

  ev_zero    <- sum(zapsmall(eigenvalues) == 0) # Number of eigenvalues equal to 0
  ev_nonzero <- length(eigenvalues) - ev_zero

  alpha <- t(nrt0) %*% solve(eigenvectors)

  comp <- (alpha[1:ev_nonzero] / eigenvalues[1:ev_nonzero]) *
    eigenvectors[1:ev_nonzero,]

  output_cw <- calculate_weights(comp, stock_values)

  modes_index <- output_cw$mode_index

  stocks_weights <- lapply(output_cw$weights, function(weights_list) {
    names(weights_list) <- stock_names
    weights_list
  })

  thetas <- lapply(output_cw$thetas, function(thetas_list) {

    if(length(thetas_list) == length(stock_names)){
      names(thetas_list) <- stock_names
    }

    thetas_list
  })

  params_vector        <- mdl$deSolve_components$consts
  par_names            <- names(params_vector)
  names(par_names)     <- par_names

  param_elas <- lapply(par_names, function(par_name) {
    new_pars           <- params_vector
    new_pars[par_name] <- new_pars[par_name] * 1.001

    simtime       <- time_DDWA:(time_DDWA + 1)
    disturbed_row <- data.frame(
      deSolve::ode(
        y = stock_values,
        times = simtime,
        func = mdl$deSolve_components$func,
        parms = new_pars,
        method = "euler")) %>% slice(1)

    vars_values <- sapply(vars_expr, function(x) with(disturbed_row, eval(x)))

    Am       <- gains_matrices$A_matrix
    Bm       <- evaluate_matrix(gains_matrices$B_matrix, disturbed_row)
    Cm       <- evaluate_matrix(gains_matrices$C_matrix, disturbed_row)
    Dm       <- evaluate_matrix(gains_matrices$D_matrix, disturbed_row)
    Um       <- solve(diag(nrow(Dm)) - Dm)
    Jacobian <- Am + Bm %*% Um %*% Cm
    db       <- ((diag(n_vars) - Dm) %*% vars_values) - (Cm %*% stock_values)
    nrt0     <- Jacobian %*% stock_values + Bm %*% Um %*% db

    eigensystem  <- eigen(Jacobian)
    eval_param   <- eigensystem$values #eigenvalues
    evec_param   <- t(eigensystem$vectors) # eigenvectors

    ev_zero    <- sum(zapsmall(eval_param) == 0) # Number of eigenvalues equal to 0
    ev_nonzero <- length(eval_param) - ev_zero

    alpha <- t(nrt0) %*% solve(evec_param)

    comp <- (alpha[1:ev_nonzero] / eval_param[1:ev_nonzero]) *
      evec_param[1:ev_nonzero,]

    output_cw <- calculate_weights(comp, stock_values)

    # stock weights for each parameter
    sw_param <- lapply(output_cw$weights, function(weights_list) {
      names(weights_list) <- stock_names
      weights_list
    })

    bmw     <- (sw_param[[1]] - stocks_weights[[1]]) / stocks_weights[[1]]
    re_elas <- (Re(eval_param[modes_index]) /
      Re(eigenvalues[modes_index])) - 1
    im_elas <- (Im(eval_param[modes_index]) /
                  Im(eigenvalues[modes_index])) - 1

    list(behaviour_mode_weight = bmw * 1000,
         re_elasticity = re_elas * 1000,
         im_elasticity = im_elas * 1000)
  })


  list(behavior_modes       = behaviour_modes_df,
       # stock equations components
       stock_equations_comp = list(
         stocks      = stock_names,
         eigenvalues = eigenvalues[output_cw$mode_index],
         constants   = as.list(output_cw$constants),
         weights     = stocks_weights,
         thetas      = thetas),
       parameter_elasticities = param_elas)
}
