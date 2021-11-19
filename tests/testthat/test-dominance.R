test_that("struc_dominance() returns the expected data frame", {

  impact_df <- data.frame(time                = c(1, 8),
                          I__x__y__f1         = c(-0.001087266, -0.953119253),
                          I__y__y__f1         = c(1.9991130, 1.3627462),
                          I__y__y__f2         = c(-1, -1),
                          pos_impact          = c(1.9991130, 1.3627462),
                          neg_impact          = c(-1.001087, -1.953119),
                          total_impact        = c(0.9980257, -0.5903731),
                          dominant_behaviour  = c(1, -1))

  actual   <- struc_dominance(impact_df)
  expected <- data.frame(time = c(1, 8),
                         dominant_pathway = c("I__y__y__f1",
                                              "I__x__y__f1,I__y__y__f2"))

  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("remove_dominated_impacts() returns the expected data frame", {

  impact_df <- data.frame(time                = c(1, 8),
                          I__x__y__f1         = c(-0.001087266, -0.953119253),
                          I__y__y__f1         = c(1.9991130, 1.3627462),
                          I__y__y__f2         = c(-1, -1),
                          pos_impact          = c(1.9991130, 1.3627462),
                          neg_impact          = c(-1.001087, -1.953119),
                          total_impact        = c(0.9980257, -0.5903731),
                          dominant_behaviour  = c(1, -1))

  pathways   <- c("I__x__y__f1", "I__y__y__f1", "I__y__y__f2")

  actual   <- remove_dominated_impacts(impact_df, pathways)

  expected <- data.frame(time        = c(1, 8),
                         I__x__y__f1 = c(NA, -0.953119253),
                         I__y__y__f1 = c(1.9991130, NA),
                         I__y__y__f2 = c(NA, -1))

  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("pathway_combn() returns the expected data frame", {

  pathways <- c("P1", "P2", "P3")

  actual <- pathway_combn(pathways)

  expected <- data.frame(combn = c("P1", "P2", "P3", "P1 + P2",
                                  "P1 + P3", "P2 + P3",
                                  "P1 + P2 + P3"),
                         n     = c(1, 1, 1, 2, 2, 2, 3))

  expect_equal(actual, expected)
})

test_that("evaluate_pathways_over_time() returns the expected data frame", {

 combn_df <- data.frame(combn = c("P1", "P2", "P3", "P1 + P2",
                                      "P1 + P3", "P2 + P3",
                                      "P1 + P2 + P3"),
                        n     = c(1, 1, 1, 2, 2, 2, 3))

 pathways_ts <- data.frame(time = c(1,8),
                           P1   = c(NA, -0.953119253),
                           P2   = c(1.9991130, NA),
                           P3   = c(NA, -1))

 actual      <- evaluate_pathways_over_time(combn_df, pathways_ts)

 row1        <- combn_df
 row1$time   <- 1
 row1$impact <- c(NA, 1.9991130, NA, NA, NA, NA, NA)

 row2        <- combn_df
 row2$time   <- 8
 row2$impact <- c(-0.953119253, NA, -1, NA, -1-0.953119253, NA, NA)

 expected    <- rbind(row1, row2)

 expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("create_opposing_df() returns the expected data frame", {

  impact_df <- data.frame(time                = c(1, 8),
                          I__x__y__f1         = c(-0.001087266, -0.953119253),
                          I__y__y__f1         = c(1.9991130, 1.3627462),
                          I__y__y__f2         = c(-1, -1),
                          pos_impact          = c(1.9991130, 1.3627462),
                          neg_impact          = c(-1.001087266, -1.953119253),
                          total_impact        = c(0.998025734, -0.590373053),
                          dominant_behaviour  = c(1, -1))

  actual <- create_opposing_df(impact_df)

  expected <- data.frame(time            = c(1, 8),
                         opposing_impact = c(-1.001087266, 1.3627462))

  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("determine_dominance() returns the expected data frame", {

  combn_df <- data.frame(combn = c("P1", "P2", "P3", "P1 + P2",
                                   "P1 + P3", "P2 + P3",
                                   "P1 + P2 + P3"),
                         n     = c(1, 1, 1, 2, 2, 2, 3))

  row1        <- combn_df
  row1$time   <- 1
  row1$impact <- c(NA, 1.9991130, NA, NA, NA, NA, NA)

  row2        <- combn_df
  row2$time   <- 8
  row2$impact <- c(-0.953119253, NA, -1, NA, -1-0.953119253, NA, NA)

  evaluated_pathways <- rbind(row1, row2)

  opposing_df <- data.frame(time            = c(1, 8),
                            opposing_impact = c(-0.001087266 - 1, 1.362746))

  actual <- determine_dominance(evaluated_pathways, opposing_df)

  expected <- data.frame(time = c(1, 8), dominant_pathway = c("P2", "P1,P3"))

  expect_equal(actual, expected, check.attributes = FALSE)
})

