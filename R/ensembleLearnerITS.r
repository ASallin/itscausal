#' Ensemble learner function
#' 
#' computes a grid search of all possible weights that minimizes the RMSE within
#' each K folds. See Chernozhukov et al. for implementation.
#' @param predictors is a matrix of predictors
#' @param k is the number of predictors that should be included in the learning
#' computation.
#' @param y is the observed y. Set to NULL if a vector of weights is given.
#' @param RMSEweights is a vector of weights for the minimization of RMSE. Could be used
#' if for example the RMSE minimizing weights were previously computed on a training
#' dataset.
#' @return
#' @export
#'
#' @examples


ensemble <- function(predictors, k, y = NULL, RMSEweights = NULL){

  predictors <- as.matrix(predictors)
  if (!is.null(RMSEweights)){y <- NULL}
  if (!is.null(y)){y <- as.matrix(y)}

  if(k == 1){
    message("Only one prediction: no ensemble needed.")
    return(list("weighted.yhat" = predictors,
                "RMSE.methods" = "",
                "RMSEweights" = 1))
    }

  # Compute MSE for each column of methods (for information)
  if(is.null(RMSEweights)){
    MSE.method <- apply(predictors, MARGIN = 2, error, y = y)
    MSE.method <- lapply(MSE.method, function(x) x$err)
    MSE.method <- do.call(rbind, MSE.method)
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
    MSE <- apply(weighted.predictors, MARGIN = 2, error, y = y)
    MSE <- lapply(MSE, function(x) x$err)

    # Check which weight combination is the winning one
    RMSEweights        <- weight[which.min(MSE), 1:k]
    names(RMSEweights) <- colnames(predictors)

    # Select column with smallest MSE
    Yhat  <- as.matrix(weighted.predictors[, which.min(MSE)])

  } else {
    Yhat <- as.matrix(
      predictors[, names(RMSEweights)] %*% as.vector(RMSEweights)
      )
  }

  # Return
  return(list("weighted.yhat" = Yhat,
              if(is.null(RMSEweights)){"RMSE.methods" = MSE.method},
              "RMSEweights" = RMSEweights))
}

