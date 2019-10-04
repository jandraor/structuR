simulate_stocks <- function(se_components, time, normalised = FALSE) {

  purrr::map_df(se_components$stocks, function(stock_name){

    constant <- se_components$constants[[stock_name]]

    if(normalised) {
      constant <- ifelse(dplyr::near(constant, 0), 1, abs(constant))
    }

    weights  <- sapply(se_components$weights, function(weight_vector){
      weight_vector[stock_name]
    })

    if(normalised) {
      weights <- weights / constant
    }

    thetas   <- sapply(se_components$thetas, function(theta_vector){
      theta_vector[stock_name]
    })

    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

    thetas <- na.omit.list(thetas)

    values <- weights * exp(Re(se_components$eigenvalues) * time) *
      sin(Im(se_components$eigenvalues) * time + thetas)


    data.frame(stringsAsFactors = FALSE,
               stock = stock_name,
               time = time,
               value = values)
  })
}


