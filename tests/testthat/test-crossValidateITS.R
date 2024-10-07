test_that("crossValidateITS() performs k-fold cross validation on a df", {
  df <- data.frame(ind = sample(1:20, 200, replace = TRUE),
                   y = rnorm(200, mean = 10, sd = 5))

  # Test whether the order of the rows is preserved in cv
  df$orig_order <- 1:nrow(df)

  # Cross-validate
  cv <- crossValidateITS(df, id = "ind", k = 5)[, "folds"]
  cv3 <- crossValidateITS(df, id = "ind", k = 3)[, "folds"]

  # Test for correct number of rows
  expect_equal(nrow(cv), 200)
  expect_equal(nrow(cv3), 200)

  # Test for correct number of folds
  expect_equal(length(unique(cv$folds)), 5)
  expect_equal(length(unique(cv3$folds)), 3)

  # Test that the order is preserved
  expect_equal(df$orig_order, 1:nrow(df))

})
