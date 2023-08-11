#' ateITS
#' Computes average treatment effect of the policy that can be aggregated in the desired way.
#' 
#' GATE and TATE and InstATE
#' Group average treatment effect and time average treatment effect and instantaneous 
#' treatment effect
#' 
#' @param forecast.object object  from `forecastITS`
#' @param ite.object object from `iteITS`
#' @param n.periods number of periods post treatment for TATE. Default = 1
#' @param groups a vector for which groups are investigated. Default is NULL: ate for the 
#'     population are computed 
#' 
#' @import dplyr data.table
#'
#' @return
#' @export
#'
#' @examples

ateITS <- function(forecast.object, ite.object, n.periods = 1, groups = NULL){

  data <- data.table(forecast.object$data)
  iteM <- ite.object$ites

  # One possible aggregation: per time period
  if (is.null(groups)){
    InstATE <- iteM[, lapply(.SD, mean), .SDcols = "ite", by = c("time")]
    InstATE.sd <- iteM[, lapply(.SD, sd), .SDcols = "ite", by = c("time")][, .(sd = ite)]

    TATE <- iteM[time %in% c(1:n.periods), lapply(.SD, mean), .SDcols = "ite"]
    TATE.sd <- iteM[time %in% c(1:n.periods), lapply(.SD, sd), .SDcols = "ite"][, .(sd = ite)]  

  } else {
    aggr <- c("time", groups)
    InstATE <- iteM[, lapply(.SD, mean), .SDcols = "ite", by = aggr]
    InstATE.sd <- iteM[, lapply(.SD, sd), .SDcols = "ite", by = aggr][, .(sd = ite)]

    TATE <- iteM[time %in% c(1:n.periods), lapply(.SD, mean), .SDcols = "ite", by = groups]
    TATE.sd <- iteM[time %in% c(1:n.periods), lapply(.SD, sd), .SDcols = "ite", by = groups][, .(sd = ite)]   
  }
  
  return(list("groups" = groups,
              "InstATE" = list("pred" = InstATE,
                               "sd" = InstATE.sd),
              "TATE" = list("pred" = TATE,
                            "sd" = TATE.sd))
  )
  }
