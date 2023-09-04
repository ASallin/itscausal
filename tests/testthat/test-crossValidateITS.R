test_that("crossValidateITS() performs k-fold cross validation on a df", {
  df <- data.frame(ind = sample(1:20, 200, replace = TRUE),
                   y = rnorm(200, mean = 10, sd = 5))

  cv <- crossValidateITS(df, id = "ind", k = 5)
  cv3 <- crossValidateITS(df, id = "ind", k = 3)

  expect_equal(nrow(cv), 200)
  expect_equal(nrow(cv3), 200)
  expect_equal(length(unique(cv$folds)), 5)
  expect_equal(length(unique(cv3$folds)), 3)

})
