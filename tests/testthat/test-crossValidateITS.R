test_that("crossValidateITS() performs k-fold cross validation on a df", {
  df <- data.frame(ind = sample(1:20, 200, replace = TRUE),
                   y = rnorm(200, mean = 10, sd = 5))

  cv <- crossValidateITS(df, id = "ind", k = 5)

  expect_equal(nrow(cv), 200)
  expect_equal(length(unique(cv$folds)), 5)

})
