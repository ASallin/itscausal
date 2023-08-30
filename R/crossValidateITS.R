#' Cross validation function
#'
#' @param data Data. Will be converted into a ´data.frame' 
#' @param id Unique identifier
#' @param k Number of cross-validation folds
#'
#' @return
#' @export
#' 
#' @importFrom data.table data.table
#'
#' @examples
#' df <- data.frame(ind = sample(1:20, 200, replace = TRUE),
#'                  y = rnorm(200, mean = 10, sd = 5))
#' crossValidateITS(df, id = "ind", k = 5)
#'
crossValidateITS <- function(data, id, k) {

    data <- data.table::data.table(data)
    idKs <- as.matrix(unique(data[, ..id]))
    split <- runif(length(idKs))

    folds   <- as.numeric(cut(split, quantile(split, probs = seq(0, 1, 1/k)),
                            include.lowest = TRUE))

    folds <- cbind(idKs, folds)
    folds <- merge(x = data, y = folds, by = id, all.x = TRUE)
    folds <- folds[, "folds"]

    return(folds)
}
