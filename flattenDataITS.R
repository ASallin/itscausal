
flattenDataITS <- function(data, index, WINDOW, STEPS, 
                           time, covariates_time, 
                           covariates_fix , 
                           id , outcome, ...) {
    
    require(tidyr)
    require(data.table)

    # Vector of indices
    stopifnot(is.vector(index))

    # Change names
    data <- data.table(data)
    
    data <- setnames(data, time, "time")
    data <- setnames(data, outcome, "y")
    data <- setnames(data, id, "ID")

    selectCols <- c("ID", "time", "y", covariates_time, covariates_fix)
    dataT <- data[, ..selectCols]

    # Reshape for each level of index
    dataFull <- lapply(index, reshapeDataITS, data = dataT, WINDOW = WINDOW, STEPS = STEPS)
    dataFull <- do.call(rbind, dataFull)

    return(dataFull)
}
