#' Create a balanced panel if many 0
#' 
#' Wrapper around `tidyr::expand` and `left_join` to create balanced panels. 
#' @param data data is in long format (multiple rows per key)
#' @param time is a string indicating the time variable in data.
#' @param covariates_time is a vector of variable names including the covariates that
#' change with time. For better prediction, it is important to include in this vector
#' the time variables (for instance, "year" or "season")
#' @param covariates_fix is a vector of individual variables that are time invariant.
#' the `forecast` does not yet implement time-varying covariates (i.e. series of other
#' variables).
#' @param key is the key that identifies units of observations.
#' @param y is a string indicating the series.
#' 
#' @import stats, tidyr
#' @return
#' @export
#'
#' @examples
#' 
#' time <- rep(c(1:50), 5)
#' id   <- rep(1:5, each = 50)
#' year <- rep(c(rep(1990, 12), rep(1991, 12), rep(1992, 12), rep(1993, 12), rep(1994, 2)), 5)
#' y    <- time + rnorm(50, 0, 1) + (time>40)*rnorm(10, 2, 2)
#' df   <- data.frame(id, time, year, y)
#' df   <- df[sample(1:250, 230), ]
#' ex   <- expandITS(data = df, time = "time", covariates_time = c("year"), key = "id", 
#'                 y = "y")

expandITS <- function(data, key, y, time, covariates_time = NULL, 
                      covariates_fix = NULL){
    
    # Cosmetics for expand
    template <- data
    data <- data.table(data)
    template <- setnames(template, time, "TIME_expand")
    template <- setnames(template, key, "ID_expand")
    #   template <- tidyr::expand(as.data.frame(data), tidyr::nesting(key, time))
    template <- tidyr::expand(as.data.frame(template), ID_expand, TIME_expand)
    template <- setnames(template, "TIME_expand", time)
    template <- setnames(template, "ID_expand", key)

  if(!is.null(covariates_time)) {
    time_invariant <- c(covariates_time, time)
    template <- left_join(template, distinct(data[, ..time_invariant]))
  }

  if(!is.null(covariates_fix)) {
    ind_invariant  <- c(covariates_fix, key)
    template <- left_join(template, distinct(data[, ..ind_invariant]))
  }

  # Fill in missing observations with 0
  balanced_panel <- left_join(template, data) %>%
      mutate(y = ifelse(is.na(y), 0, y))  %>% 
      data.table()

  return(balanced_panel)
}