# RMSE Error function ------------------------------------------------------------

error <- function(yhat, y){
  err         <- sqrt(mean((yhat-y)^2, na.rm = T))
  mis         <- sum(abs(as.numeric(yhat > .5)-(as.numeric(y))))/length(y)   
  
  return(list("err" = err, "mis" = mis));
}
