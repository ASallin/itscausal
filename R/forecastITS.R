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
#' @param covariates_fix is a string vector of individual variables that are time invariant.
#' @param covariates_var is a string vector of individual variables that are time variant.
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
#' 
#' time <- rep(c(1:50), 5)
#' id   <- rep(1:5, each = 50)
#' year <- rep(c(rep(1990, 12), rep(1991, 12), rep(1992, 12), rep(1993, 12), rep(1994, 2)), 5)
#' y    <- time + rnorm(50, 0, 1) + (time>40)*rnorm(10, 2, 2)
#' df   <- data.frame(id, time, year, y)
#' f    <- forecastITS(data = df, time = "time", INDEX = 40, covariates_time = c("year"), key = "id", 
#'                 y = "y")
#' 
#' # Unbalanced panel
#' dfUB <- df[sample(1:250, 230), ]
#' fUB <- forecastITS(data = df, time = "time", INDEX = 40, covariates_time = c("year"), key = "id", 
#'                 y = "y")

forecastITS <- function(data, time, INDEX = 0L, WINDOW = 12L, STEPS = as.integer(WINDOW/3),
                        covariates_time, covariates_fix = NULL, covariates_var = NULL,
                        key, y, method = c("lm", "rf", "xgboost"), K = 5, CYCLE = 12L, FORECASTUNITS = NULL) {


  cv = ID = rbindlist = NULL # due to NSE notes in R CMD check

  # Preparation
  window <- WINDOW
  steps  <- STEPS
  vars <- c(time, y, key)
  if(!is.null(covariates_fix)) {vars <- c(vars, covariates_fix)}
  if(!is.null(covariates_var)) {vars <- c(vars, covariates_var)}

  if(is.null(covariates_time)) {
    data$timeCOV <- data[[time]]
    covariates_time <- c("timeCOV")
  }
  
  vars <- c(vars, covariates_time)
  data <- data.table(data)
  data <- data[, ..vars]
  data <- data[, time := time - INDEX]

  # Warnings and errors
  if (!all(vars %in% colnames(data))) {
    stop("Some variables are not found in the data.")
  }
  

  if (nrow(na.omit(data)) != nrow(data)) {
    stop("NAs have been found. Please remove.")
  }

  # Check whether balanced panel
  if(min(data[, .N, by = key]$N) !=  length(unique(data$time))){
    warning("It seems that you don't have a balanced panel. Please have a look at your df.")
    data <- expandITS(data, key, y, time, covariates_time, covariates_fix)
  }

  # Number of forecast units
  if (is.null(FORECASTUNITS)) FORECASTUNITS <- max(data$time) + steps

  # Prepare folds for cross-validation
  data[, "cv"] <- crossValidateITS(data, id = key, k = K)

  forecast <- list()

  for (i in 1:K){

    message(i, "...")

    index_max <- abs(min(data[[time]])) - (window + steps)

    dat <- flattenDataITS(data = data,
                          index = c(0:(index_max)),
                          WINDOW = window,
                          STEPS = steps,
                          time = time,
                          covariates_time = covariates_time,
                          covariates_fix = if(is.null(covariates_fix)) {
                                              c("cv")
                                            } else {c(covariates_fix, "cv")}, 
                          covariates_var = covariates_var,
                          key = key,
                          outcome = y)

    exclude_x <- !names(dat) %in% c(key, time, "cv", outcome)
   
    x.train  = dat[dat$cv != i, exclude_x]
    x.test   = dat[dat$cv == i, exclude_x]
    y.train  = dat[dat$cv != i, outcome]
    y.test   = dat[dat$cv == i, outcome]

    results <- fit(x.train  = x.train,
                   x.test   = x.test,
                   y.train  = y.train,
                   y.test   = y.test,
                   method   = method)

    # Predict for future (from INDEX - steps to ForecastUNITS)
    # All observations are in the support of our data, which means
    # we have the observed y as well as the covariates_var.
    pred <- flattenDataITS(data = data[data$cv == i,],
                           index = c(0:-FORECASTUNITS),
                           WINDOW = window,
                           STEPS = steps,
                           time = time,
                           covariates_time = covariates_time,
                           covariates_var = covariates_var,
                           covariates_fix = if(is.null(covariates_fix)) {
                                              c("cv")
                                            } else {c(covariates_fix, "cv")},
                           key = key, outcome = y)

    x.pred   = pred[, exclude_x]

    # test
    models = results$models
    lPred <- split(pred, pred$time)
    ly <- split(pred$y, pred$time)
    lPred   = lapply(lPred, function(x) x[, exclude_x])
    predictionList <- list()

    for(ii in 1:length(lPred)){
      # Extract series of prediction from "models". 
      # Models has the same size as methods.
      prediction <- matrix(ncol = 0, nrow = nrow(lPred[[ii]]))
    
      # Predict on x using models
      for (jj in models) {
          prediction <- cbind(prediction, predict(jj, x = lPred[[ii]]))
      }

      colnames(prediction) <- sapply(models, function(x) x@type)
    
      # Take the ensemble of the predictions in the train period given the weights.
      if(length(RMSEweights) > 1){
        prediction <- prediction[, names(RMSEweights)] %*% as.vector(RMSEweights)
      }
    
      predictionList[[ii]] <- prediction
      if(ii == length(lPred)) {break} else {lPred[[ii + 1]]$LAG1 <- as.numeric(prediction)}
    }

    prediction <- do.call(cbind, predictionList)
    
    test <- cbind(prediction, split(pred, pred$time)[[1]][[key]])
    colnames(test) <- c(paste0("PRED", split(pred, pred[[key]])[[1]][[time]]), key)

    test <- melt(data.table(test), id.vars = key, variable = time, value = outcome)
    test[[time]] <- as.numeric(gsub("PRED", "", test[[time]]))

    # prediction <- stepcastITS(models = results$models,
    #                           x = x.pred,
    #                           # x = pred,
    #                           STEPS = steps,
    #                           RMSEweights = results$weights,
    #                           covariates_time = covariates_time)



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
    # HERE: FLAG! We take the median of all predictions per time period.
    # Here: use post intervention periods for weighting of predictions!
    predicted <- list_predictionsT[,lapply(.SD, mean, na.rm=TRUE),
                                   by = c("ID")][, -"time"]

    predicted <- melt(predicted, id.vars = c('ID'), variable.name = "time",
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
  ret <- list(
    "forecast" = forecast,
    "out" = dfPred,
    "data" = data,
    "RMSEweights" = results$weights,
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
    ))
  return(ret)
}