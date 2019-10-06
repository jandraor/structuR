generate_elasticities_table <- function(param_elasticities, stock_name) {
  param_names <- names(param_elasticities)
  weights     <- sapply(param_elasticities, function(param_obj) {
    param_obj$behaviour_mode_weight[stock_name]
  })

  Re_Elasticities <- purrr::map_dbl(param_elasticities, "re_elasticity")
  Im_Elasticities <- purrr::map_dbl(param_elasticities, "im_elasticity")

  data.frame(parameters = param_names, weight = weights,
             Re_elasticity = Re_Elasticities,
             Im_elasticity = Im_Elasticities,
             row.names = NULL)
}
