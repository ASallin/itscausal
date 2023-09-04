test_that("Function `fit` works", {
  
  n <- 1000
  x <- matrix(rnorm(n * 4), ncol = 4)
  colnames(x) <- paste0("x", 1:4)

  y <- x[, 1] + 2 * x[, 2] + rnorm(n)

  x.train <- x[1:750,]
  y.train <- y[1:750]
  x.test  <- x[751:1000,]
  y.test  <- y[751:1000]

  fitLM  <- fit(x.train, y.train, x.test, y.test, method = "lm")
  fitRF  <- fit(x.train, y.train, x.test, y.test, method = "rf")
  fitXGB <- fit(x.train, y.train, x.test, y.test, method = "xgboost")
  fit    <- fit(x.train, y.train, x.test, y.test, method = c("lm", "rf", "xgboost"))  
  fit1   <- fit(x.train, y.train, x.test, y.test, method = c("lm", "xgboost", "rf"))  
  
  expect_equal(length(fitLM$models), 1)
  expect_equal(length(fitRF$models), 1)
  expect_equal(length(fitXGB$models), 1)
  expect_equal(length(fit$models), 3)
  expect_equal(length(fit$weights), 3)
  expect_equal(names(fit$weights), c("lm", "rf", "xgboost"))
  expect_equal(names(fit1$weights), c("lm", "rf", "xgboost"))
})
