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

iteITS <- function(forecast.object){

  data <- data.table(forecast.object$data)
  key  <- forecast.object$call$key
  time <- forecast.object$call$time
  covariates_fix <- forecast.object$call$covariates_fix

  # Extract forecast: bind all rows from the stepcast predictions
  fit <- do.call(rbind, lapply(forecast.object$forecast, function(x) x[[2]]))

  # Create a LONG table of predicted values
  t <-melt(fit, id.vars = c(key, time), variable.name = "PRED",
                      value.name = "prediction")
  t <- t[, time := (gsub("PRED", "", PRED))]
  t <- t[, time := as.numeric(time)]
  t <- t[, c("PRED"):= NULL]
  t <- na.omit(t)
  t$pred <- 1

  # Join with original data. Keep only years where prediction is done.
  m <- dplyr::left_join(data, t, by = c(key, time), multiple = "all")
  m <- na.omit(m)
  
  # Compute ite as difference between prediction and observed value
  m$ite <- m$prediction - m[[y]]
  m <- m[time > -1, ]
  
  # We have multiple predictions per id per time period due to the structure of 
  # stepcast. For now, average out all of them. Could also do a weighted average
  # given rmse, or given proximity to cut-off.
  if(!is.null(covariates_fix)){
    vector_pred <- m[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c(key, time, covariates_fix)]
    vector_sd   <-  m[, lapply(.SD, sd), by = c(key, time, covariates_fix), .SDcols = "ite"][, .(sd = ite)]
  } else {
    vector_pred <- m[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c(key, time)]
    vector_sd   <-  m[, lapply(.SD, sd), by = c(key, time), .SDcols = "ite"][, .(sd = ite)]    
  }
  
  iteM <- cbind(vector_pred, vector_sd)

  return(list(ites = iteM))
  }