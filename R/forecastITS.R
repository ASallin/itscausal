#' Forecast function for ITS
#' 
#' Main function for the ITS. Uses cross-validation to perform "sliding-window" in
#' single-step or multiple-step estimation of time series.
#' @param data data is in long format (multiple rows per key)
#' @param time is a string indicating the time variable in data.
#' @param INDEX is an integer indicating the time point where the treatment begins.
#' Default is index = 0, with pre-treatment periods having time < 0.
#' @param WINDOW is an integer indicating the number of time periods before time t
#' are used for prediction. Ideally, if time is in month, WINDOW should include a full
#' seasonality cycle (i.e. WINDOW = 12 if in months)
#' @param STEPS is an integer indicating the number of time periods used to assess fit
#' before the intervention.
#' @param covariates_time is a vector of variable names including the covariates that
#' change with time. For better prediction, it is important to include in this vector
#' the time variables (for instance, "year" or "season")
#' @param covariates_fix is a vector of individual variables that are time invariant.
#' the `forecast` does not yet implement time-varying covariates (i.e. series of other
#' variables).
#' @param key is the key that identifies units of observations.
#' @param y is a string indicating the series.
#' @param method is a vector containing the prediction methods used for prediction.
#' Possible methods are c("lm", "rf").
#' @param K is the number of folds for the k-fold cross-validation technique.
#' Default is K = 5.
#' 
#' @import stats
#' @return
#' @export
#'
#' @examples


forecastITS <- function(data, time, INDEX = 0L, WINDOW = 12L, STEPS = as.integer(WINDOW/3),
                        covariates_time, covariates_fix,
                        key, y, method = c("lm", "rf"), K = 5, CYCLE = 12L, FORECASTUNITS = NULL) {


  cv = ID = rbindlist = NULL # due to NSE notes in R CMD check

  # Preparation
  window <- WINDOW
  steps  <- STEPS
  vars <- c(time, y, key, covariates_fix, covariates_time)

  # Prepare data
  data <- data.table(data)

  if (!all(vars %in% colnames(data))) {
    stop("Some variables are not found in the data.")
  }
  data <- data[, ..vars]
  data <- data[, time := time - INDEX]


  # Number of forecast units
  if (is.null(FORECASTUNITS)) FORECASTUNITS <- max(data$time) + steps


  # Prepare folds for cross-validation
  data[, "cv"] <- crossValidateITS(data, id = "id", k = K)

  forecast <- list()

  for (i in 1:K){

    message(i, "...")

    dat <- flattenDataITS(data = data,
                          index = c(0:window),
                          WINDOW = window,
                          STEPS = steps,
                          time = time,
                          covariates_time = covariates_time,
                          covariates_fix = c(covariates_fix, "cv"),
                          key = key,
                          outcome = y)

    x.train  = dat[cv != i, -c("ID", "time", "cv", "y")]
    x.test   = dat[cv == i, -c("ID", "time", "cv", "y")]
    y.train  = dat[cv != i, "y"]
    y.test   = dat[cv == i, "y"]

    results <- fit(x.train  = x.train,
                   x.test   = x.test,
                   y.train  = y.train,
                   y.test   = y.test,
                   method   = method)

    # Predict for future
    pred <- flattenDataITS(data = data[cv == i,],
                           index = c(0:-FORECASTUNITS),
                           WINDOW = window,
                           STEPS = steps,
                           time = time,
                           covariates_time = covariates_time,
                           covariates_fix = c(covariates_fix, "cv"),
                           key = key, outcome = y)

    x.pred   = pred[, -c("ID", "time", "cv", "y")]

    prediction <- stepcastITS(models = results$models,
                           x.test = x.pred,
                           STEPS = steps,
                           RMSEweights = results$weights)

    colnames(prediction) <- paste0("LEAD", 1:steps)

    test <- cbind(prediction, pred[,c("ID", "time")])

    list_predictions <- split(test, test$time)


    flattenPrediction <- function(x){
      colnames(x)[1:steps] <- paste0("PRED", 1:steps + as.numeric(unique(x$time)))
      return(x)
    }
    list_predictionsT <- lapply(list_predictions, flattenPrediction)
    list_predictionsT <- rbindlist(list_predictionsT, fill = TRUE)
    list_predictionsT <- setcolorder(list_predictionsT, c("ID", "time"))

    # HERE: FLAG! We take the mean of all predictions per time period.
    predicted <- list_predictionsT[,lapply(.SD, mean, na.rm=TRUE),
                                   by = c("ID")][, -"time"]

    predicted <-melt(predicted, id.vars = c('ID'), variable.name = "time",
                     value.name = "prediction")

    predicted <- predicted[, time := (gsub("PRED", "", time))]
    predicted <- predicted[, time := as.numeric(time)]

    return_list <- list(list_predictions, list_predictionsT, predicted)

    forecast[[i]] <- return_list
  }

  # Prepare final dataset
  dfPred <- do.call(rbind, lapply(forecast, function(x) x[[3]]))
  dfPred <- dfPred[order(time, ID), ]

  dfPred <- setnames(dfPred, "time", time)
  dfPred <- setnames(dfPred, "ID", key)
  dfPred <- setnames(dfPred, "prediction", paste0(y, "_hat"))


  # Return
  return(list(
    "forecast" = forecast,
    "out" = dfPred,
    "data" = data,
    "call" = list("time" = time,
                  "INDEX" = INDEX,
                  "WINDOW" = WINDOW,
                  "STEPS" = STEPS,
                  "covariates_time" = covariates_time,
                  "covariates_fix" = covariates_fix,
                  "key" = key,
                  "y" = y,
                  "method" = method,
                  "K" = K,
                  "CYCLE" = CYCLE,
                  "FORECASTUNITS" = FORECASTUNITS
    )))
}
