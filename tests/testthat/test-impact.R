test_that("impact() returns the expected string", {
  o <- "z"
  d <- "x"

  v_df <- data.frame(stock    = c("x", "y", "z"),
                     equation = c("-R * x * z",
                                  " R * x * z - a * y",
                                  "a * y - z"))

  actual <- impact(o, d, v_df)

  expect_type(actual, "character")

  expected <- "-(R * x) * (a * y - z) / (-R * x * z)"
  expect_equal(actual, expected)
})

test_that("impacts_on() returns the expected data.frame", {

  pathways_df <- data.frame(
    origin      = c("z", "x", "z", "y", "x", "y", "z"),
    destination = c("x", "x", "y", "y", "y", "z", "z"))

  v_df <- data.frame(stock    = c("x", "y", "z"),
                     equation = c("-R * x * z",
                                  " R * x * z - a * y",
                                  "a * y - z"))

  actual <- impacts_on("x", pathways_df, v_df)

  expect_s3_class(actual, "data.frame")

  I_z_x <- "-(R * x) * (a * y - z) / (-R * x * z)"
  I_x_x <- "-(R * z) * (-R * x * z) / (-R * x * z)"


  expected <- data.frame(origin      = c("z", "x"),
                         destination = c("x", "x"),
                         impact      = c(I_z_x, I_x_x))

  expect_equal(actual, expected)
})

test_that("evaluate_impact() returns the expected data.frame", {

  I_z_x <- "-(R * x) * (a * y - z) / (-R * x * z)"
  I_x_x <- "-(R * z) * (-R * x * z) / (-R * x * z)"


  impact_df <- data.frame(origin      = c("z", "x"),
                          destination = c("x", "x"),
                          impact      = c(I_z_x, I_x_x))

  x0  <- 0.974182771
  x10 <- 0.466656479
  y0  <- 0.009831036
  y10 <- 0.078797261
  z0  <- 0.007933189
  z10 <- 0.078490487

  sim_df <- data.frame(
    time = c(0L, 10L),
    x = c(x0, x10),
    y = c(y0, y10),
    z = c(z0, z10),
    R = c(2L, 2L),
    a = c(1L, 1L))

  actual <- evaluate_impact(impact_df, sim_df)

  expect_s3_class(actual, "data.frame")

  e1 <-  -(2 *x0) * (y0 - z0) / (-2 * x0 * z0)
  e2 <-  -(2 *x10) * (y10 - z10) / (-2 * x10 * z10)
  e3 <-  - 2 * z0
  e4 <-  - 2 * z10

  expect_equal(actual[1, 2], e1)
  expect_equal(actual[2, 2], e2)
  expect_equal(actual[1, 3], e3)
  expect_equal(actual[2, 3], e4)

  actual_names   <- colnames(actual)
  expected_names <- c("time", "I_z_x", "I_x_x")
  expect_equal(actual_names, expected_names)
})
