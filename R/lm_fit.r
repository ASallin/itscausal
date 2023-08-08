#' This function estimates random forest lasso regression based on the \code{\link{ranger}} package
#'
#' @param x Matrix of covariates (number of observations times number of covariates matrix)
#' @param y vector of outcomes (no df format)
#' @param args List of arguments passed to  \code{\link{ranger}}
#' @import ranger
#' 
#' @return
#' @export
#'
#' @examples
#'
#' @keywords internal

lm_fit = function(x, y, args=list()) {
  
  data.ols <- as.data.frame(cbind(y, x)) 
  names(data.ols)[1] <- "Y"
  
  ols <- do.call(lm,
                 c(list(paste("Y ~ ."),
                 data = data.ols), 
                args))
  
  rm(data.ols )
  return(ols)
}


#' Prediction based on ranger
#' @param lm_fit Output of \code{\link{ranger}} or \code{\link{ranger_fit}}
#' @param x Covariate matrix of training sample
#' @param xnew Covariate matrix of test sample
#'
#' @return Returns list containing:
#' \item{prediction}{vector of predictions for xnew}
#'
#' @keywords internal

predict.lm_fit = function(lm_fit, x, xnew = NULL) {
  
  if (is.null(xnew)) xnew <- x
  
  data.ols <- as.data.frame(xnew)
  
  fit_lm <- as.vector(predict(lm_fit, newdata = data.ols) )

  return(fit_lm)
  
  } 

