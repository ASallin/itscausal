#' factor_to_dummy
#'
#' Function to transform dummy variables into factors.
#'
#' @param data a data.frame or a data.table
#' @param factor_vector_names a vector of strings for the names of variables
#' needed to be converted.
#'
#' @return a data frame
#'
#' @examples
#' data <- data.frame("y" = sample(c(0, 0:2), 1000, TRUE),
#'                    "year" = c(rep(1:3, 333), 3),
#'                    "age" = sample(c(18, 25, 30, 40, 50, 60), 1000, TRUE),
#'                    "gender" = sample(c(0,1), 1000, TRUE),
#'                    "region" = sample(1:5, 1000, TRUE)
#'                    )
#' x <- policyrate(var = "y", time.var = "year", pop.group = "age", policy.group = "region",
#'                 data = data)
#' subset(x, "year==1")
#'
#'
#' @export


factor_to_dummy <- function(data = data.frame(), factor_vector_names = c()){

  data <- as.data.frame(data)

  new <- model.matrix.lm(as.formula(paste0("~ -1 + ", paste0(factor_vector_names, collapse = "+"))),
                         data, na.action = "na.pass")

  data <- data[, !(names(data) %in% factor_vector_names)]
  data <- cbind(data, new)

  return(data)
}
