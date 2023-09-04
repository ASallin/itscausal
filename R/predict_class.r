#' MLModelITS Class
#'
#' This class contains a ML model for prediction. It allows to train a model,
#' and to predict on a test sample.
#'
#' @slot model is the model contained in the class.
#' @slot type gives the type of the ML algorithm used.
#' @export
#' 
#' @example 
#' # Set fake data
#' n <- 1000
#' x <- matrix(rnorm(n * 4), ncol = 4)
#' colnames(x) <- paste0("x", 1:4)
#' y <- x[, 1] + 2 * x[, 2] + rnorm(n)
#' 
#' # Example usage
#' rf_model <- MLModelITS(NULL, "rf")
#' rf_model <- train(rf_model, y = y, x = x)
#' rf_model <- train(rf_model, y = y, x = x,
#'             tuning_params = list(respect.unordered.factors = "order",
#'                                     num.trees = 5000L,
#'                                     classification = FALSE,
#'                                     importance = "impurity"))
#' predict.MLModelITS(rf_model, x)                                    
#' 
#' lm_model <- MLModelITS(NULL, "lm")
#' lm_model <- train.MLModelITS(lm_model, y = y, x = x)
#' predict.MLModelITS(lm_model, x)
#' 
#' xg_model <- MLModelITS(NULL, "xgboost")
#' xg_model <- train.MLModelITS(xg_model, y = y, x = x)
#' xg_model <- train.MLModelITS(xg_model, y = y, x = x, tuning_params = list(max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "regression"))
#' predict.MLModelITS(xg_model, x)
 

# Define a new S3 class
setClass("MLModelITS",
  slots = c(
    model = "ANY",
    type = "character"
  )
)


#' Constructor function for creating MLModel objects
#' 
#' Creates instances of class MLModelITS
#' @param model is the model
#' @param type is the type of algorithm used 
#' @return
#' @export
#'
#' @examples

MLModelITS <- function(model, type) {
  new("MLModelITS", model = model, type = type)
}


#' TrainITS
#' 
#' Define the train function as an S3 generic
#' @param object object to train on 
#' @return
#' @export
#'
#' @examples
#' 
train <- function(object, ...) {
  UseMethod("train")
}



#' train.MLModelITS
#' 
#' Define the train function as an S3 generic
#' @param object takes object of class `MLModelITS`
#' @param x a matrix of predictors
#' @param y a vector of target
#' @param tuning_params a list containing tuning parameters for algorithms
#' @return
#' @export
#' @importFrom methods new slot
#' @examples
#' 

train.MLModelITS <- function(object, x, y, tuning_params = NULL, ...) {

    model_type <- slot(object, "type")

  if (model_type == "rf") {
    data.ranger <- as.data.frame(cbind(y, x)) 
    names(data.ranger)[1] <- "y"
    
    object@model <- do.call(ranger::ranger,
                                c(list(paste("y ~ ."),
                                data = data.ranger), 
                            tuning_params))

  } else if (model_type == "lm") {
    data.ols <- as.data.frame(cbind(y, x)) 
    names(data.ols)[1] <- "y"
    
    object@model <- lm(y ~ ., data = data.ols, ...)

  } else if (model_type == "lasso") {
    # data.lasso <- as.data.frame(cbind(y, x)) 
    # names(data.lasso)[1] <- "y"
    # object@model <- glmnet::glmnet(x, y, alpha = 1, ...)

  } else if (model_type == "xgboost") {
    if (is.null(tuning_params)) {
      
      object@model <- xgboost::xgboost(data = x, label = y, nrounds = 3, verbose = 0, ...)

    } else {
        object@model <- do.call(xgboost::xgboost, 
                                c(list(data = x, label = y, verbose = 0),
                                tuning_params))
    }
    
  } else {
    stop("Unsupported model type")
  }
  
  return(object)
}


#' predict.MLModelITS
#' @param object Output of \code{\link{ranger}} or \code{\link{ranger_fit}}
#' @param x Covariate matrix of training sample
#' @param xnew Covariate matrix of test sample
#'
#' @return Returns list containing:
#' \item{prediction}{vector of predictions for xnew}
#'
#' @keywords internal


# Predict method for the MLModel class
predict.MLModelITS <- function(object, x, xnew = NULL, ...) {

    model_type <- slot(object, "type")

    if (is.null(xnew)) xnew <- x
    data <- as.data.frame(xnew)
    
    if (model_type == "rf") {
        return(predict(object@model, data, type = "response")$predictions)

    } else if (model_type == "lm") {
        return(suppressWarnings(as.vector(predict(object@model, newdata = data))))

    } else if (model_type == "lasso") {
        # return(predict(object@model, newx = data))

    } else if (model_type == "xgboost") {
        return(predict(object@model, as.matrix(data)))

    } else {
        stop("Unsupported model type")
    }
}



