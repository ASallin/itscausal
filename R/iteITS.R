#' iteITS
#' Computes individual treatment effect of the policy that can be aggregated in the desired way.
#'
#' @param forecast.object object  from `forecastITS`
#'
#' @import dplyr data.table
#'
#' @return
#' @export
#'
#' @examples
iteITS <- function(forecast.object) {

  data <- data.table(forecast.object$data)
  key <- forecast.object$call$key
  time <- forecast.object$call$time
  y <- forecast.object$call$y
  covariates_fix <- forecast.object$call$covariates_fix

  # Extract forecast: bind all rows from the stepcast predictions
  fit <- do.call(rbind, lapply(forecast.object$forecast, function(x) x[["y_hat"]]))
  names(fit)[names(fit) == "ID"] <- key
  names(fit)[names(fit) == "time"] <- time
  fit <- data.table(fit)

  # Create a LONG table of predicted values
  t <- melt(fit,
    id.vars = c(key, time), variable.name = "PRED",
    value.name = "prediction"
  )
  t <- data.table(t)
  t$PRED <- NULL
  t <- na.omit(t)
  t$pred <- 1

  # Join with original data. Keep only years where prediction is done.
  m <- dplyr::left_join(data, t, by = c(key, time), multiple = "all")
  m <- na.omit(m)

  # Compute ite as difference between prediction and observed value
  m$ite <- m$prediction - m[[y]]
  m <- m[time > 0, ]

  # We have multiple predictions per id per time period due to the structure of
  # stepcast. For now, average out all of them. Could also do a weighted average
  # given rmse, or given proximity to cut-off.
  by_cols <- if (!is.null(covariates_fix)) c(time, covariates_fix) else c(time)
  iteM <- m[, .(ite = mean(ite), sd = stats::sd(ite)), by = by_cols]

  return(list(ites = iteM))
}
