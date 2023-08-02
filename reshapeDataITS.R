#' reshapeDataITS
#'
#' `reshapeDataITS` prepares a panel data or time-series data for the single or multiple
#' step forecast of counterfactuals.
#' @INDEX is a vector 



reshapeDataITS <- function(INDEX, WINDOW, STEPS = 1, data){
                        
    if(!is.integer(WINDOW)){stop("`WINDOW` must be an integer.")}

    data <- data.table(data)

    # Keep observations with dates earlier than index but later than index - past_window
    dataWindow <- data[time <= -(INDEX + STEPS) & (time >= (-(INDEX + STEPS) - WINDOW)), ]

    # Long to wide with sum aggregation
    dfaw <- if(is.null(covariates_fix)) {
        dcast(dataWindow, ID ~ time, value.var = "y")
    } else {
        dcast(dataWindow, paste(" ID +", paste(covariates_fix, collapse = " + "), "~", time ), 
              value.var = "y")
    }

    colnames(dfaw) <- c("ID", covariates_fix, paste0("LAG", WINDOW:1), "y")

    selectCols <- c("time", covariates_time)
    dfaw <- cbind(dfaw, data[time == -(INDEX + STEPS), ..selectCols])

    # Return
    return(dfaw) 
}

