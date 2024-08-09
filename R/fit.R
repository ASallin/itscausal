#' Multi-step forecasting function
#'
#' @param x.train the matrix of predictors from the train dataset
#' @param y.train the vector of target variable from the train dataset
#' @param x.test the matrix of predictors from the test dataset
#' @param y.test the vector of target variable from the test dataset
#' @param binary binary variable indicating whether the target `y` is a binary
#' variable. If yes, ...
#'
#' @param method vector of method names. Methods available are "lm", "rf", "lasso".
#'
#' @import data.table
#'          xgboost
#'          ranger
#'          glmnet
#'
#' @return
#' @export
#'
#' @keywords internal
#'
#' @examples
#' n <- 1000
#' x <- matrix(rnorm(n * 4), ncol = 4)
#' colnames(x) <- paste0("x", 1:4)
#'
#' y <- x[, 1] + 2 * x[, 2] + rnorm(n)
#'
#' x.train <- x[1:750, ]
#' y.train <- y[1:750]
#' x.test <- x[751:1000, ]
#' y.test <- y[751:1000]
#'
#' fit(x.train, y.train, x.test, y.test, method = "lm")
fit <- function(x.train, y.train, x.test, y.test,
                binary = F,
                method = c("lasso", "lm", "rf", "xgboost"),
                optim = FALSE) {
    if (binary == T) {
        family <- "binomial"
    } else {
        family <- "gaussian"
    }

    x.train <- data.matrix(x.train)
    x.test <- data.matrix(x.test)
    y.train <- data.matrix(y.train)
    y.test <- data.matrix(y.test)

    models <- list()
    m_results <- matrix(nrow = nrow(x.train), ncol = 0)
    m_results_test <- matrix(nrow = nrow(x.test), ncol = 0)

    for (ii in method) {
        # Arguments
        if ("lasso" %in% ii) {
            args <- list(
                penalty = "lasso",
                alpha = 1,
                ncores = parallel::detectCores(),
                output.time = T
            )
        } else if ("rf" %in% ii) {
            args <- list(
                respect.unordered.factors = "order",
                num.trees = 2000,
                classification = FALSE,
                importance = "impurity"
            )
        } else if ("xgboost" %in% ii & isFALSE(optim)) {
            args <- list(
                max.depth = 5,
                eta = 1, nthread = 2, nrounds = 9, objective = "reg:squarederror"
            )
        } else if ("xgboost" %in% ii & isTRUE(optim)) {
            grid <- expand.grid(
                max_depth = c(5, 7, 9),
                eta = c(0.01, 0.1, 0.3),
                nthread = 2,
                nrounds = c(50, 150, 300),
                objective = "reg:squarederror"
            )
            t <- apply(grid, 1, function(row) {
                list(
                    max_depth = as.numeric(row["max_depth"]),
                    eta = as.numeric(row["eta"]),
                    nthread = as.numeric(row["nthread"]),
                    nrounds = as.numeric(row["nrounds"]),
                    objective = as.character(row["objective"])
                )
            })

            predlist <- list()

            warning("Optimizing hyperparameters for xgboost.")

            predList <- lapply(
                t,
                function(x) {
                    model <- MLModelITS(NULL, ii)
                    model <- train(model,
                        y = y.train,
                        x = x.train,
                        tuning_params = x
                    )
                }
            )

            # Predict on train sample
            m_rmse <- lapply(predList, predict, x = x.train)

            # Minimal RMSE
            winner <- which.min(lapply(
                lapply(m_rmse, error, y = y.train),
                function(x) x$err
            ))

            args <- t[winner][[1]]
        }

        # Create instance of S4 object
        model <- MLModelITS(NULL, ii)

        # Train on training sample
        model <- train(model, y = y.train, x = x.train, tuning_params = args)

        # Predict on train sample
        m_results <- cbind(m_results, predict(model, x = x.train))

        # Predict on test sample
        m_results_test <- cbind(m_results_test, predict(model, x = x.test))

        # Save models
        models[[ii]] <- model
    }

    colnames(m_results_test) <- c(method)

    # Compute the weighting that minimizes RMSE
    diag_rmse <- ensemble(
        predictors = m_results_test,
        k = length(method),
        y = y.test,
        RMSEweights = NULL
    )

    y.test.weight <- diag_rmse$weighted.yhat

    # Return
    list_results <- list(
        "y.test.weight" = y.test.weight,
        "weights" = diag_rmse$RMSEweights,
        "models" = models
    )

    return(list_results)
}
