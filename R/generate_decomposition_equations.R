generate_decomposition_equations <- function(se_components) {
  purrr::map_df(se_components$stocks, function(stock_name){
    constant <- se_components$constants[[stock_name]] %>% round(2)

    weights  <- sapply(se_components$weights, function(weight_vector){
      weight_vector[stock_name]
    }) %>% round(2)

    thetas   <- sapply(se_components$thetas, function(theta_vector){
      theta_vector[stock_name]
    })

    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

    thetas <- na.omit.list(thetas) %>% round(2)


    euler_terms <- sapply(se_components$eigenvalues, function(eigenvalue){
      real_part_ev <- round(Re(eigenvalue), 2)
      paste0("e^{", real_part_ev," \\tau}")
    })

    img_value <- sapply(se_components$eigenvalues, function(eigenvalue) {
      if(Im(eigenvalue) == 0) return (NULL)
      Im(eigenvalue)
    })

    thetas_sign <- ifelse(sign(thetas) == 1, "+", "-")

    polar_expression <- paste0("sin(", round(img_value, 2), "\\tau",
                               thetas_sign, abs(thetas),")")

    weights_sign <- ifelse(sign(weights) == 1, "+", "-")

    equation <- paste("$",constant, weights_sign, abs(weights), "\\times", euler_terms,
                      "\\times", polar_expression, "$")

    data.frame(stringsAsFactors = FALSE,
               stock    = stock_name,
               equation = equation)
  })
}
