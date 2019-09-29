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
          theta[index] - 2*pi
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
       thetat = theta_list,
       ct = ct,
       mode_index = mode_index)
}


run_DDWA <- function(time_DDWA, sim_df, gains_matrices, graph) {
  row <- dplyr::filter(sim_df, time == time_DDWA)

  vars_names         <- igraph::V(graph)[type == "variable"]$name
  vars_expr          <- lapply(vars_names, rlang::parse_expr)
  vars_values        <- sapply(vars_expr, function(x) with(row, eval(x)))
  names(vars_values) <- vars_names
  n_vars             <- length(vars)

  stock_names         <- igraph::V(graph)[type == "stock"]$name
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


  ev_zero    <- sum(zapsmall(eigenvalues) == 0) # Number of eigenvalues equal to 0
  ev_nonzero <- length(eigenvalues) - ev_zero

  alpha <- t(nrt0) %*% solve(eigenvectors)

  comp <- (alpha[1:ev_nonzero] / eigenvalues[1:ev_nonzero]) *
    eigenvectors[1:ev_nonzero,]

  output_cw <- calculate_weights(comp, stock_values)
}
