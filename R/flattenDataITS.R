
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
                           time, covariates_time,
                           covariates_fix,
                           key , outcome) {


    # Vector of indices
    stopifnot(is.vector(index))


    # Change names
    data <- data.table(data)

    data <- setnames(data, time, "time")
    data <- setnames(data, outcome, "y")
    data <- setnames(data, key, "ID")

    selectCols <- c("ID", "time", "y", covariates_time, covariates_fix)
    dataT <- data[, ..selectCols]


    # Function to reshape
    reshapeDataITS <- function(index, WINDOW, STEPS, dataT,
                                covariates_fix,
                                covariates_time){

        # Keep observations with dates earlier than index but later than index - past_window
        dataWindow <- dataT[time <= -(index + STEPS) & (time >= (-(index + STEPS) - WINDOW)), ]

        # Long to wide with sum aggregation
        if(is.null(covariates_fix)) {
            dfaw <- data.table::dcast(dataWindow, ID ~ time, value.var = "y")
        } else {
            dfaw <- dcast(dataWindow, as.formula(paste(" ID +", paste(covariates_fix, collapse = " + "), "~", time )),
                            value.var = "y")
        }

        # colnames(dfaw) <- c("ID", covariates_fix, paste0("LAG", -c(-(WINDOW+STEPS):-(index+STEPS+1))), "y")
        colnames(dfaw) <- c("ID", covariates_fix, paste0("LAG", WINDOW:1), "y")
        # colnames(dfaw) <- c("ID", covariates_fix, paste0("LAG", colnames(dfaw)[colnames(dfaw) %in% -WINDOW:1]), "y")

        selectCols <- c("time", covariates_time)
        dfaw <- cbind(dfaw, dataT[time == -(index + STEPS), ..selectCols])

        # Return
        return(dfaw)
    }


    # Reshape for each level of index
    dataFull <- lapply(index, reshapeDataITS,
                        data = dataT, WINDOW = WINDOW, STEPS = STEPS,
                        covariates_time = covariates_time, covariates_fix = covariates_fix)
    dataFull <- do.call(rbind, dataFull)

    return(dataFull)
}
