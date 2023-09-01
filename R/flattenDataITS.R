
#' flattenDataITS
#'
#' @param data Dataset
#' @param index Index for intervention
#' @param WINDOW Window for prediction
#' @param STEPS
#' @param time
#' @param covariates_time
#' @param covariates_fix
#' @param key
#' @param outcome
#'
#' @return
#' @export
#'
#' @examples
flattenDataITS <- function(data, index, WINDOW, STEPS,
                           time, 
                           covariates_time,
                           covariates_fix,
                           covariates_var = NULL,
                           key , outcome) {


    # Vector of indices
    stopifnot(is.vector(index))

    # Change names
    data <- as.data.frame(data)

    # data <- setnames(data, time, "time")
    # data <- setnames(data, outcome, "y")
    # data <- setnames(data, key, "ID")

    selectCols <- c(key, time, outcome, covariates_time, covariates_fix, covariates_var)
    dataReshape <- data[, selectCols]


    # Function to reshape
    reshapeDataITS <- function(index, WINDOW, STEPS, dataReshape,
                                covariates_fix = NULL,
                                covariates_time,
                                covariates_var = NULL){

        # Keep observations with dates earlier than index but later than index - past_window
        # dataWindow <- dataReshape[time <= -(index + STEPS) & (time >= (-(index + STEPS) - WINDOW)), ]
        dataWindow <- dataReshape[dataReshape[[time]] <= -(index + STEPS) & (dataReshape[[time]] >= (-(index + STEPS) - WINDOW)), ]
        
        # Long to wide with sum aggregation
       if(is.null(covariates_fix) & is.null(covariates_var)) {
            
            formulaDCast <- as.formula(paste0(key, "~ ", time))
            
            dfWide <-  dcast(as.data.table(dataWindow), formulaDCast, value.var = outcome)

            colnames(dfWide) <- c(key, paste0("LAG", WINDOW:1), outcome)

        } else if (!is.null(covariates_fix) & is.null(covariates_var))  {
            
            formulaDCast <- as.formula(paste0(key, "+", paste(covariates_fix, collapse = " + "), 
                                                "~", time))

            dfWide <-  dcast(as.data.table(dataWindow), formulaDCast, value.var = outcome)

            colnames(dfWide) <- c(key, covariates_fix, paste0("LAG", WINDOW:1), outcome)

        } else if (!is.null(covariates_fix) & !is.null(covariates_var)) {
        
            formulaDCast <- as.formula(paste0(key, "+", paste(covariates_fix, collapse = " + "), 
                                                "~ ", time))

            dfWide <-  dcast(as.data.table(dataWindow), formulaDCast, value.var = c(outcome, covariates_var))

            nams <- character()
            for (i in covariates_var) {nams <- c(nams, paste0(i, "LAG", WINDOW:0))} 

            colnames(dfWide) <- c(key, covariates_fix, 
                                paste0("LAG", WINDOW:1),
                                outcome,
                                nams)

        } else { stop() }

        selectCols <- c("time", covariates_time)
        dfWide <- cbind(dfWide, dataReshape[dataReshape[[time]] == -(index + STEPS), selectCols])

        # Return
        return(dfWide)
    }


    # Reshape for each level of index
    dataFull <- lapply(index, reshapeDataITS,
                        data = dataReshape, WINDOW = WINDOW, STEPS = STEPS,
                        covariates_time = covariates_time, 
                        covariates_fix = covariates_fix,
                        covariates_var = covariates_var)
    dataFull <- do.call(rbind, dataFull)

    return(dataFull)
}
