test_that("multiplication works", {
  time <- rep(c(1:50), 5)
  id   <- rep(1:5, each = 50)
  year <- rep(c(rep(1990, 12), rep(1991, 12), rep(1992, 12), rep(1993, 12), rep(1994, 2)), 5)
  y    <- time + rnorm(50, 0, 1) + (time>40)*rnorm(10, 2, 2)
  df   <- data.frame(id, time, year, y)
   
  f <- forecastITS(data = df, time = "time", INDEX = 40, covariates_time = c("year"), key = "id", 
                   y = "y")

  expect_equal(nrow(f$data), 250)
  expect_equal(f$call$covariates_fix, NULL)
})
