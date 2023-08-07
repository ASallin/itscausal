
# Lasso/Ridge/Enet -------------------------------------------------------------------

#' This function estimates cross-validated lasso regression based on the \code{\link{glmnet}} package
#'
#' @param x Matrix of covariates (number of observations times number of covariates matrix)
#' @param y vector of outcomes (no df format)
#' @param args List of arguments passed to  \code{\link{glmnet}}
#' @import biglasso
#'
#' @return An object with S3 class "biglasso"
#'
#' @keywords internal

lasso_fit = function(x, y, args=list()) {
  
  if(is.null(args$penalty)){stop("Penalty must be 'lasso', 'ridge', 'enet'.")}
  if(is.null(args$family)){stop("Family must be 'binomial' or 'gaussian'.")}
  if(is.null(args$alpha)){stop("Alpha must be 1 for lasso, 0 for ridge, ]0,1[ for enet.")}
  
  x.big <- as.big.matrix(x)
  
  message(paste0("Training biglasso for ", args$penalty, "..."))
  lasso <- do.call(cv.biglasso,
                   c(list(X = x.big,
                          y = y, 
                          verbose = F),
                     args))
  
  rm(x.big)
  return(lasso)
}


#' Prediction based on Lasso.
#' @param lasso_fit Output of \code{\link{biglasso}} or \code{\link{lasso_fit}}
#' @param x Covariate matrix of training sample
#' @param xnew Covariate matrix of test sample
#'
#' @return Returns list containing:
#' \item{prediction}{vector of predictions for xnew}
#'
#' @keywords internal

predict.lasso_fit = function(lasso_fit, x, xnew = NULL) {
  
  if (is.null(xnew)) xnew <- x
  
  x.big <- as.big.matrix(xnew)
  
  fit <- predict(lasso_fit, 
                 X = x.big, 
                 type = "response", lambda = lasso_fit[["lambda.min"]])
  
  return(as.matrix(fit))
}



