test_that("clip_matrix() set boundary on a value above 1", {
  m_expected <- matrix(c(0, 1, 0, 0), nrow = 2)
  m_test     <- matrix(c(0, 5, 0, 0), nrow = 2)
  clipped_m  <- clip_matrix(m_test, 0, 1)
  expect_equal(clipped_m, m_expected)
})

test_that("clip_matrix() set boundary on a value below 0", {
  m_expected <- matrix(c(0, 0, 0, 0), nrow = 2)
  m_test     <- matrix(c(0, 0, -1, 0), nrow = 2)
  clipped_m  <- clip_matrix(m_test, 0, 1)
  expect_equal(clipped_m, m_expected)
})
