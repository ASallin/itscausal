stepcast <- function(models, x.test, STEPS = 1L,
                     RMSEweights){
  
  updateStep <- function(x.test, models, RMSEweights){
    
    prediction <- lapply(models, 
      function(mod, x = x.test){
        if(!exists(mod$attr)) stop("no attribute")
        if(mod$attr == "lm")    pred = cbind("lm" = predict.lm_fit(mod[[1]], x))
        if(mod$attr == "rf")    pred = cbind("rf" = predict.ranger_fit(mod[[1]], x))
        if(mod$attr == "lasso") pred = cbind("lasso" = predict.lasso_fit(mod[[1]], x))
        return(pred)
      }
    )
    prediction <- do.call(cbind, prediction)
    prediction <- prediction[, names(RMSEweights)] %*% as.vector(RMSEweights)
    
    # Update the dataframe for new prediction
    b <- colnames(x.test)
    new.x <- x.test
    new.x[, grep("LAG", colnames(new.x))] <- cbind(
          new.x[, grep("LAG", colnames(new.x)), with = FALSE ][, -1],
          prediction
          )
    new.x <- new.x[, ":="(year = year + 1, season = season + 1)]
    
    return(list("prediction" = prediction,
                "newDF" = new.x))
}

stepPredictions <- matrix(nrow = nrow(y.test),
                          ncol = STEPS)

for (jj in 1:STEPS){
  step <- updateStep(x.test, models, RMSEweights = RMSEweights)
  stepPredictions[, jj] <- step$prediction
  x.test <- step$newDF
}

return(stepPredictions)
}
