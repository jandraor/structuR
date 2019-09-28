evaluate_matrix <- function(expressionMatrix, values_df) {
  nrows <- nrow(expressionMatrix)
  ncols <- ncol(expressionMatrix)
  evaluated_matrix <- matrix(NA, nrow = nrows, ncol = ncols) #empty matrix
  rownames(evaluated_matrix) <- rownames(expressionMatrix)
  colnames(evaluated_matrix) <- colnames(expressionMatrix)

  for(i in 1:nrows) {

    for(j in 1:ncols){
      expr_elem <- expressionMatrix[i,j]
      eval_elem <- with(values_df, eval(expr_elem))
      evaluated_matrix[i, j] <- eval_elem
    }
  }
  evaluated_matrix
}
