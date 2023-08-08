#' RMSE
#' 
#' Computes the RMSE. 
#'
#' @param yhat Vector of predicted values
#' @param y Vector of "true" values
#'
#' @return
#' @export
#' 
#' @import stats
#'
#' @examples
#'
error <- function(yhat, y){
  err         <- sqrt(mean((yhat-y)^2, na.rm = T))
  mis         <- sum(abs(as.numeric(yhat > .5)-(as.numeric(y))))/length(y)   
  
  return(list("err" = err, "mis" = mis));
}
