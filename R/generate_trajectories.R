generate_trajectories <- function(se_components, time, normalised = FALSE) {

  purrr::map_df(se_components$stocks, function(stock_name){
    constant <- se_components$constants[[stock_name]]

    if(normalised) {
      normalising_constant <- ifelse(dplyr::near(constant, 0), 1, abs(constant))
      constant             <- 0
    }



    weights  <- sapply(se_components$weights, function(weight_vector){
      weight_vector[stock_name]
    })

    if(normalised) {
      weights <- weights / normalising_constant
    }

    thetas   <- sapply(se_components$thetas, function(theta_vector){
      theta_vector[stock_name]
    })

    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

    thetas <- na.omit.list(thetas)

    values <- constant + weights * exp(Re(se_components$eigenvalues) * time) *
      sin(Im(se_components$eigenvalues) * time + thetas)


    data.frame(stringsAsFactors = FALSE,
               stock = stock_name,
               time = time,
               value = values)
  })
}


