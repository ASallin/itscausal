# Multi-step forecasting function
fit <- function(x.train, y.train, x.test, y.test, binary = F, 
                        method = c("lasso", "lm", "rf")){
    
    # method <- match.arg(method)
    if(binary == T){family <- "binomial"} else {family <- "gaussian"}
  
    x.train <- data.matrix(x.train)
    x.test  <- data.matrix(x.test)
    y.train <- data.matrix(y.train)
    y.test  <- data.matrix(y.test)

    # Arguments
    args.lasso <- 
      list(penalty = "lasso",
           alpha = 1,
           ncores = parallel::detectCores(),
           output.time = T)

    args.rf <- 
      list(respect.unordered.factors = "order",
           num.trees = 2000,
           classification = FALSE,
           importance = "impurity")


    # Train on training sample
    if("lasso" %in% method) {fit_lasso <- lasso_fit(x.train, y.train, args = c(args.lasso, family = family))}
    if("lm" %in% method)    {fit_lm    <- lm_fit(x.train, y.train)}
    if("rf" %in% method)    {fit_rf    <- ranger_fit(x.train, y.train, args = args.rf)}


    # Predict on train sample
    m_results <- matrix(nrow = nrow(x.train))
    
    if("lasso" %in% method) {m_results <- cbind(m_results, "lasso" = predict.lasso_fit(fit_lasso, x.train, xnew = x.train))}
    if("lm" %in% method)    {m_results <- cbind(m_results, "lm" = predict.lm_fit(fit_lm, x.train, xnew = x.train))}
    if("rf" %in% method)    {m_results <- cbind(m_results, "rf" = predict.ranger_fit(fit_rf, x.train, xnew = x.train))}


    # Predict on test sample
    m_results_test <- matrix(nrow = nrow(x.test))

    if("lasso" %in% method) {m_results_test <- cbind(m_results_test, "lasso" = predict.lasso_fit(fit_lasso, x.train, xnew = x.test))}
    if("lm" %in% method)    {m_results_test <- cbind(m_results_test, "lm" = predict.lm_fit(fit_lm, x.train, xnew = x.test))}
    if("rf" %in% method)    {m_results_test <- cbind(m_results_test, "rf" = predict.ranger_fit(fit_rf, x.train, xnew = x.test))}


    # Compute the weighting that minimizes RMSE
    diag_rmse <- ensemble(predictors = m_results_test, 
                          k = length(method), 
                          y = y.test)

    y.test.weight <- diag_rmse$weighted.yhat

    # Return
    models <- list(if (exists("fit_lasso")) list(fit_lasso, "attr" = "lasso"),
                   if (exists("fit_lm"))    list(fit_lm, "attr" = "lm"),
                   if (exists("fit_rf"))    list(fit_rf, "attr" = "rf")
                   )

    models <- models[lengths(models) != 0]

    list_results <- list("y.test.weight" = y.test.weight,
                         "weights" = diag_rmse$RMSEweights,
                         "models" = models
                        )

    return(list_results)

}
