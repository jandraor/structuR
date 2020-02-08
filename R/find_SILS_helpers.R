clip_matrix <- function(m, lower, upper) {
  clipped_m <- apply(m, 2, function(row) pmax(lower, pmin(row, upper)))
  rownames(clipped_m) <- colnames(clipped_m)
  clipped_m
}
