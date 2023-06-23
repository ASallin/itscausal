# RMSE Error function ------------------------------------------------------------

error <- function(yhat, y){
  err         <- sqrt(mean((yhat-y)^2, na.rm = T))
  mis         <- sum(abs(as.numeric(yhat > .5)-(as.numeric(y))))/length(y)   
  
  return(list(err = err, mis = mis));
}



# Learner function --------------------------------------------------------
#' computes a grid search of all possible weights that minimizes the RMSE within
#' each K folds. See Chernozhukov et al. for implementation. 
#' @param predictors is a matrix of predictors
#' @param k is the number of predictors that should be included in the learning 
#' computation. 

ensemble <- function(predictors, k, y = NULL, RMSE.weights = NULL){
  
  # Lots of NaN with Ridge. Change them to 0 in the matrix.
  predictors <- round(predictors, 10) 
  # predictors[is.nan(predictors)] = Inf
  
  # Compute MSE for each column of methods (for information)
  if(is.null(RMSE.weights)){
    MSE.method <- apply(predictors, MARGIN = 2, error, y = y) %>% 
        map(., pluck("err")) %>% 
        do.call(rbind, .)
    MSE.method <- cbind(rownames(MSE.method), MSE.method) 
    rownames(MSE.method) <- NULL
  
    # Grid search    
    # Take only the k methods with lowest RMSE
    predictors <- predictors[, order(MSE.method[, 2], decreasing = F)[1:k]]
    
    # Prepare grid search
    if(k < 4)  {lst     <- lapply(numeric(k), function(x) as.vector(seq(0, 1, 0.01))) }
    if(k == 4) {lst     <- lapply(numeric(k), function(x) as.vector(seq(0, 1, 0.04))) }
    if(k == 5) {lst     <- lapply(numeric(k), function(x) as.vector(seq(0, 1, 0.05))) }
    if(k > 5)  {lst     <- lapply(numeric(k), function(x) as.vector(seq(0, 1, 0.10))) }
  
    gr                  <- as.matrix(expand.grid(lst))
    weight              <- gr[rowSums(gr)==1,]
    rm(gr)
  
    # Weight predictors
    weighted.predictors <- predictors %*% t(weight)
  
    # Compute MSE for each column of the weighted predictions
    MSE <- apply(weighted.predictors, MARGIN = 2, error, y = y) %>% 
      map(., pluck("err")) 
  
    # Check which weight combination is the winning one
    RMSE.weights        <- weight[which.min(MSE), 1:k]
    names(RMSE.weights) <- colnames(predictors)
  
    # Select column with smallest MSE
    Yhat  <- weighted.predictors[, which.min(MSE)] %>% 
      as.matrix()
    
  } else {
    Yhat <- predictors[, names(RMSE.weights)] %*% as.vector(RMSE.weights) %>% 
      as.matrix()
  }
  
  # Return
  return(list("weighted.yhat" = Yhat,
              "RMSE.methods" = MSE.method,
              "RMSE.weights" = RMSE.weights))
}

