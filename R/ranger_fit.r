# RF with Ranger ----------------------------------------------------------

#' This function estimates random forest lasso regression based on the \code{\link{ranger}} package
#'
#' @param x Matrix of covariates (number of observations times number of covariates matrix)
#' @param y vector of outcomes (no df format)
#' @param args List of arguments passed to  \code{\link{ranger}}
#' @import ranger
#'
#' @return An object with S3 class "ranger"
#'
#' @keywords internal

ranger_fit = function(x, y, args=list()) {
  
  data.ranger <- as.data.frame(cbind(y, x)) 
  names(data.ranger)[1] <- "Y"
  
  rf <- do.call(ranger,
                c(list(paste("Y ~ ."),
                       data = data.ranger), 
                  args))
  
  rm(data.ranger)
  return(rf)
}


#' Prediction based on ranger
#' @param lasso_fit Output of \code{\link{ranger}} or \code{\link{ranger_fit}}
#' @param x Covariate matrix of training sample
#' @param xnew Covariate matrix of test sample
#'
#' @return Returns list containing:
#' \item{prediction}{vector of predictions for xnew}
#'
#' @keywords internal

predict.ranger_fit = function(ranger_fit, x, xnew = NULL) {
  
  if (is.null(xnew)) xnew <- x
  
  data.ranger <- as.data.frame(xnew)
  
  fit_ranger <- predict(ranger_fit,
                        data = data.ranger,
                        type = "response")$predictions
  return(fit_ranger)
  
  } 

