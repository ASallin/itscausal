test_that("forecastITS works", {
  smallSimulation <- function(n, t, index) {
    time <- rep(c(1:t), n)
    id <- rep(1:n, each = t)
    u_i <- rep(rnorm(n, 0, 1), each = t)
    year <- rep(c(rep(1:(t %/% 12), each = 12), rep(t %/% 12 + 1, each = t %% 12)), n)
    x1 <- rep(sample(c(0, 1), n, replace = TRUE), each = t)
    x2 <- rep(sample(c(1, 2, 3), n, replace = TRUE), each = t)
    xv1 <- rep(sample(c(0, 1), t, replace = TRUE), n)
    xv2 <- rep(sample(c(1, 2, 3), t, replace = TRUE), n)
    y <- time + rnorm(t, 0, 1) + (time > index) * rnorm(t, 2, 2) + u_i

    df <- cbind(time, id, year, y, x1, x2, xv1, xv2)

    return(df)
  }

  f <- forecastITS(
    data = smallSimulation(n = 10, t = 20, index = 11),
    time = "time", INDEX = 11, covariates_time = c("year"),
    key = "id",
    y = "y", WINDOW = 7, K = 3
  )

  fA <- forecastITS(
    data = smallSimulation(n = 100, t = 50, index = 25),
    time = "time", INDEX = 25, covariates_time = c("year"),
    key = "id",
    y = "y", WINDOW = 12, K = 5
  )

  f1 <- forecastITS(
    data = smallSimulation(n = 10, t = 35, index = 25),
    time = "time", INDEX = 35, covariates_time = c("year"), key = "id",
    y = "y",
    covariates_fix = c("x1", "x2"),
    covariates_var = c("xv1", "xv2")
  )

  # Unbalanced panel
  df <- smallSimulation(n = 10, t = 25, index = 20)
  dfUB <- df[sample(1:250, 230), ]
  fUB <- forecastITS(
    data = dfUB, time = "time", INDEX = 20, covariates_time = c("year"), key = "id",
    y = "y"
  )

  expect_equal(nrow(f$data), 200)
  expect_equal(nrow(fA$data), 5000)
  expect_equal(nrow(f1$data), 350)
  expect_equal(nrow(fUB$data), 250)
  expect_equal(f$call$covariates_fix, NULL)
  expect_equal(f1$call$covariates_fix, c("x1", "x2"))
})
