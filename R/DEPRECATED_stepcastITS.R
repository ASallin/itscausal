#' stepcastITS
#'
#' @param models
#' @param x
#' @param STEPS
#' @param RMSEweights
#'
#' @return
#' @export
#' 
#' @keywords internal
#'
#' @examples

stepcastITS <- function(models, x, STEPS = 1L,
                        RMSEweights,
                        covariates_fix, covariates_time){
  
  # Predict and update the dataset 
  updateStep <- function(x, models, RMSEweights){
    prediction <- matrix(nrow = nrow(x))

    for (jj in models) {
        prediction <- cbind(prediction, predict(jj, x = x))
    }

    prediction <- as.matrix(prediction[, 2:ncol(prediction)])
    colnames(prediction) <- sapply(models, function(x) x@type)

    if(length(RMSEweights) > 1){
      prediction <- prediction[, names(RMSEweights)] %*% as.vector(RMSEweights)
    }

    # Update the dataframe for new prediction
    b <- colnames(x)
    new.x <- x
    new.x[, grep("LAG", colnames(new.x))] <- cbind(
      new.x[, grep("LAG", colnames(new.x)), with = FALSE ][, -1],
      prediction
    )

    # Update by one the time variables
    new.x <- new.x[, (covariates_time) := lapply(.SD, function(x) x + 1), 
                    .SDcols = covariates_time]
    # new.x$year <- new.x$year + 1
    # new.x$season <- new.x$season + 1

    return(list("prediction" = prediction,
                "newDF" = new.x))
  }

  stepPredictions <- matrix(nrow = nrow(x),
                            ncol = STEPS)

  for (jj in 1:STEPS){
    st <- updateStep(x, models, RMSEweights = RMSEweights)
    stepPredictions[, jj] <- st$prediction
    x <- st$newDF
  }

  return(stepPredictions)
}
