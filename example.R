library(ranger)
library(biglasso)
library(bigmemory)
library(purrr)
install.packages("biglasso")

data = df
WINDOW = 12L
covariates_time = c("year", "season")
covariates_fix = c("X")
outcome = "y"
id = "id"
time = "time"
index = c(1:10)
INDEX = 1
STEPS = 5L

source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/flattenDataITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/reshapeDataITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/crossValidateITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/lm_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/lasso_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/ranger_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/forecastITS.R")


a <- flattenDataITS(data = data, 
                    index = c(1:10), 
                    WINDOW = 12L,
                    STEPS = 5L,
                    time = "time",
                    covariates_time = c("year", "season"), 
                    covariates_fix = "X",
                    id = "id", outcome = "y")


K <- 5
a[, "cv"] <- crossValidateITS(a, id = "ID", k = K)


# Test
forecast <- list()

for (i in 1:K){

    cat(i, "...")

    results <- forecastITS(x.train = a[cv != i, -c("ID", "time", "cv", "y")], 
                           x.test = a[cv == i, -c("ID", "time", "cv", "y")], 
                           y.train = a[cv != i, "y"], 
                           y.test = a[cv == i, "y"],
                           method = c("lm", "rf"))

    # check error
    diag_rmse <- ensemble(predictors = results$y.test, k = 2, y = y.test)

    # y.train.ensemble <- ensemble(results$y.train, k = 2, y = y.train, RMSE.weights = diag_rmse$RMSE.weights)
    # y.test.ensemble <- ensemble(results$y.test, k = 2, y = y.test, RMSE.weights = diag_rmse$RMSE.weights)


    # Apply this to the analysis dataset for prediction
    a_forecast_1 <- flattenDataITS(data = data, 
                                   index = c(-1), 
                                   WINDOW = 12L,
                                   STEPS = 1L,
                                   time = "time",
                                   covariates_time = c("year", "season"), 
                                   covariates_fix = "X",
                                   id = "id", outcome = "y")

    # Apply prediction from fold Ki to all observations in forecast.
    results_1 <- forecastITS(x.train = a_forecast_1[, -c("ID", "time", "y")], 
                             x.test  = a_forecast_1[, -c("ID", "time", "y")],
                             y.train = a_forecast_1[, "y"], 
                             y.test  = a_forecast_1[, "y"],
                             method = c("lm", "rf"))

    # Apply the weights from the prediction
    forecast[[i]] <- list("weights" = ensemble(results_1$y.test, k = 2, RMSE.weights = diag_rmse$RMSE.weights)$RMSE.weights,
                          "mat"     = cbind(a_forecast_1[, c("ID", "time", "year", "season", "X")],
                                            "yLead" = ensemble(results_1$y.test, k = 2, RMSE.weights = diag_rmse$RMSE.weights)[[1]])
                         )
}

do.call(rbind, map(forecast, pluck,  "weights"))

yLead1 <- map(forecast, "mat", 1)  %>% 
                map(., ~.x[, 6])  %>% 
                do.call(cbind, .)  %>% 
                apply(., 1, mean)
dataL1 <- cbind(forecast[[1]]$mat[,1:6], yLead1, "post" = 1)
dataL1 <- dataL1[ , -"yLead.V1"]
dataL1 <- dataL1  %>% 
    rename(id = ID,
           y = yLead1)

data <- rbind( 
            cbind(data, "pred" = 0),
            cbind(dataL1, "pred" = 1))
data <- data  %>% arrange(id, time)

View(data)


df  %>% 
  group_by(time)  %>% 
  summarise(y = mean(y))  %>% 
  ggplot(aes(y = y, x = time)) +
  geom_line() +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_classic()
# Now, we set the y hat as new outcome and conduct multi-step prediction
