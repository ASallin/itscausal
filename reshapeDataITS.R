data = df
INDEX = 0
WINDOW = 12L
covariates_time = c("year", "season")
covariates_fix = c("X")
outcome = "y"
id = "id"
TIME = "time"

reshapeDataITS <- function(data, INDEX, WINDOW, TIME,
                           covariates_time, covariates_fix,
                           id, outcome){
  
  require(data.table)
  require(lubridate)
  require(tidyverse)
  
  if(!is.integer(WINDOW)){stop("`WINDOW` must be an integer.")}
  
  data <- data.table(data)
  
  # Change names
  setnames(data, TIME, "TIME")
  setnames(data, outcome, "y")
  setnames(data, id, "ID")

  # Keep observations with dates earlier than index but later than index - past_window
  dataWindow <- data[TIME < INDEX & (TIME >= (INDEX - WINDOW)), ]
  
  # Long to wide with sum aggregation
  dfaw <- pivot_wider(dataWindow,
                      id_cols = c(ID, covariates_fix), 
                      names_from = TIME,
                      values_from = y,
                      names_prefix = "y_lag")

  colnames(dfaw) <- make.names(colnames(dfaw))
  
  selectCols <- c("TIME", covariates_time)
  dfaw <- cbind(dfaw, data[data$TIME == INDEX, ..selectCols])
  
  
  # Return
  return(list("data" = df_pred,
              "aux" = list("INDEX" = INDEX,
                           "WINDOW" = WINDOW,
                           "surv" = Surv(df_pred$time_to_OP, df_pred$event)),
              "id" = id))
  
}

