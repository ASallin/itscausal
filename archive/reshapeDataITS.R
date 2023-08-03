#' reshapeDataITS
#'
#' `reshapeDataITS` prepares a panel data or time-series data for the single or multiple
#' step forecast of counterfactuals.
#' @INDEX is a vector 

# data = df
# data[, "cv"] <- crossValidateITS(data, id = "id", k = K)
#  data <- setnames(data, time, "time")
#     data <- setnames(data, outcome, "y")
#     data <- setnames(data, id, "ID")
# WINDOW = 12L
# covariates_time = c("year", "season")
# covariates_fix = c("X", "cv")
# outcome = "y"
# id = "id"
# time = "time"
# index = c(1:10)
# INDEX = 1
# STEPS = 5L

reshapeDataITS <- function(INDEX, WINDOW, STEPS = 1L, data, 
                           covariates_fix = NULL,
                           covariates_time = NULL){
                        
    if(!is.integer(WINDOW)){stop("`WINDOW` must be an integer.")}

    data <- data.table(data)

    # Keep observations with dates earlier than index but later than index - past_window
    dataWindow <- data[time <= -(INDEX + STEPS) & (time >= (-(INDEX + STEPS) - WINDOW)), ]

    # Long to wide with sum aggregation
    dfaw <- if(is.null(covariates_fix)) {
        dcast(dataWindow, ID ~ time, value.var = "y")
    } else {
        dcast(dataWindow, as.formula(paste(" ID +", paste(covariates_fix, collapse = " + "), "~", time )), 
              value.var = "y")
    }

    colnames(dfaw) <- c("ID", covariates_fix, paste0("LAG", WINDOW:1), "y")

    selectCols <- c("time", covariates_time)
    dfaw <- cbind(dfaw, data[time == -(INDEX + STEPS), ..selectCols])

    # Return
    return(dfaw) 
}

