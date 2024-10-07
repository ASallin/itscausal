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
#' @param y
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
                           key, y) {
  # Vector of indices
  stopifnot(is.vector(index))

  # Change names
  data <- as.data.frame(data)

  # Prepare df
  selectCols <- c(key, time, y, covariates_time, covariates_fix, covariates_var)
  data_out <- data[, selectCols]


  # Function to reshape
  reshapeDataITS <- function(index, WINDOW, STEPS, dataToReshape,
                             covariates_fix = NULL,
                             covariates_time,
                             covariates_var = NULL) {
    # Keep observations with dates earlier than index but later than index - past_window
    cond <- dataToReshape[[time]] <= index-STEPS & dataToReshape[[time]] >= ((index - STEPS) - WINDOW)
    dataWindow <- dataToReshape[cond, ]

    # Long to wide with sum aggregation
    if (is.null(covariates_fix) & is.null(covariates_var)) {

      formulaDCast <- as.formula(paste0(key, "~ ", time))
      dfWide <- dcast(as.data.table(dataWindow), formulaDCast, value.var = y)
      colnames(dfWide) <- c(key, paste0("LAG", WINDOW:1), y)

    } else if (!is.null(covariates_fix) & is.null(covariates_var)) {

      formulaDCast <- as.formula(paste0(
        key, "+", paste(covariates_fix, collapse = " + "),
        "~", time
      ))

      # Time = -Steps is y, the rest is lagged
      dfWide <- dcast(as.data.table(dataWindow), formulaDCast, value.var = y)

      colnames(dfWide) <- c(key, covariates_fix,
                            paste0("LAG", WINDOW:1), y)

    } else if (!is.null(covariates_fix) & !is.null(covariates_var)) {
      formulaDCast <- as.formula(paste0(
        key, "+", paste(covariates_fix, collapse = " + "),
        "~ ", time
      ))

      dfWide <- dcast(
        as.data.table(dataWindow),
        formulaDCast,
        value.var = c(y, covariates_var)
      )

      nams <- character()
      for (i in covariates_var) {
        nams <- c(nams, paste0(i, "LAG", WINDOW:0))
      }

      colnames(dfWide) <- c(key, covariates_fix, paste0("LAG", WINDOW:1), y, nams)
    } else {
      stop()
    }

    selectCols <- c("time", covariates_time)
    dfWide <- cbind(dfWide,
                    dataToReshape[dataToReshape[[time]] == index - STEPS, selectCols]
                    )

    # Return
    return(dfWide)
  }


  # Reshape for each level of index
  dataFull <- lapply(index, reshapeDataITS,
                     dataToReshape = data_out,
                     WINDOW = WINDOW, STEPS = STEPS,
                     covariates_time = covariates_time,
                     covariates_fix = covariates_fix,
                     covariates_var = covariates_var
  )
  dataFull <- do.call(rbind, dataFull)

  # Transform season variables to factor
  for (col in covariates_time) {
      dataFull[[col]] <- as.factor(dataFull[[col]])
  }

  formula <- as.formula(paste("~", paste(covariates_time, collapse = " + "), "- 1"))
  time_dummies <- model.matrix(formula, data = dataFull)
  dataFull <- cbind(dataFull, time_dummies)
  dataFull[[covariates_time]] <- NULL

  return(as.data.frame(dataFull))
}
