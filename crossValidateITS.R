# Cross validation function
crossValidateITS <- function(data, id, k) {
    
    idKs <- as.matrix(unique(data[, ..id]))
    split <- runif(length(idKs))
    
    folds   <- as.numeric(cut(split, quantile(split, probs = seq(0, 1, 1/k)),
                            include.lowest = TRUE)) 

    folds <- cbind(idKs, folds)
    folds <- merge(x = data, y = folds, by = id, all.x = TRUE)
    folds <- folds[, "folds"]

    return(folds)
}
