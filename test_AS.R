# Create data
time <- rep(c(1:50), 5)
id <- rep(1:10, each = 50)
year <- rep(c(rep(1990, 12), rep(1991, 12), rep(1992, 12), rep(1993, 12), rep(1994, 2)), 5)
y <- time + rnorm(50, 0, 1) + (time > 40) * rnorm(10, 2, 2)
x1 <- rep(sample(c(0, 1), 5, replace = T), each = 50)
x2 <- rep(sample(c(1, 2, 3), 5, replace = T), each = 50)
xv1 <- rep(sample(c(0, 1), 50, replace = T), 5)
xv2 <- rep(sample(c(1, 2, 3), 50, replace = T), 5)
month <- rep(c(1:12), length.out = length(time))

df <- cbind(time, id, year, month, y, x1, x2, xv1, xv2)


# Define the different values to test
steps_values <- c(1, NULL, 10)
window_values <- c(NULL, 2, 4)
index_values <- c(12L, 25L, 35L)
method_values <- list(c("lm"), c("lm", "xgboost"))

# Loop through each combination of STEPS, WINDOW, and INDEX
for (steps in steps_values) {
  for (window in window_values) {
    for (index in index_values) {
      for (method in method_values) {

        cat("steps:", steps, " w:", window, "index;", index, "\n")

        # Check if the function runs without error
        forecastITS(
          data = df,
          time = "time",
          key = "id",
          y = "y",
          INDEX = index,
          WINDOW = window,
          STEPS = steps,
          covariates_time = c("month"),
          covariates_fix = NULL,
          method = method,
          K = 5,
          optim = FALSE
        )
        # The NA here means you expect no error
      }
    }
  }
}
