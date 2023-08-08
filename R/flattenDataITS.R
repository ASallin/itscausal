
#' flattenDataITS
#'
#' @param data
#' @param index
#' @param WINDOW
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


    if(!is.integer(WINDOW)){stop("`WINDOW` must be an integer.")}
    if(!is.integer(STEPS)){stop("`STEPS` must be an integer.")}

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
    reshapeDataITS <- function(index, WINDOW, STEPS, data,
                                covariates_fix,
                                covariates_time){

        # Keep observations with dates earlier than index but later than index - past_window
        dataWindow <- data[time <= -(index + STEPS) & (time >= (-(index + STEPS) - WINDOW)), ]

        # Long to wide with sum aggregation
        dfaw <- if(is.null(covariates_fix)) {
            dcast(dataWindow, ID ~ time, value.var = "y")
        } else {
            dcast(dataWindow, as.formula(paste(" ID +", paste(covariates_fix, collapse = " + "), "~", time )),
                  value.var = "y")
        }

        colnames(dfaw) <- c("ID", covariates_fix, paste0("LAG", WINDOW:1), "y")

        selectCols <- c("time", covariates_time)
        dfaw <- cbind(dfaw, data[time == -(index + STEPS), ..selectCols])

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
