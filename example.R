set.seed(281929)

library(ranger)
library(biglasso)
library(bigmemory)
library(purrr)

source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/simulation.R")
data = df
# WINDOW = 12L
# covariates_time = c("year", "season")
# covariates_fix = c("X", "cv")
# outcome = "y"
# id = "id"
# time = "time"
# index = c(1:10)
# INDEX = 1
# STEPS = 5L

source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/flattenDataITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/crossValidateITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/lm_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/lasso_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/ranger_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/forecastITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/ensembleLearnerITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/rmse.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/stepcastITS.R")

K <- 5
data[, "cv"] <- crossValidateITS(data, id = "id", k = K)

i <- 2

steps = 20L
window = 12L

# Test
forecast <- list()

for (i in 1:K){

    message(i, "...")

    a <- flattenDataITS(data = data, 
                        index = c(1:10), 
                        WINDOW = window,
                        STEPS = steps,
                        time = "time",
                        covariates_time = c("year", "season"), 
                        covariates_fix = c("X", "cv"),
                        id = "id", 
                        outcome = "y")
    
    x.train  = a[cv != i, -c("ID", "time", "cv", "y")]
    x.test   = a[cv == i, -c("ID", "time", "cv", "y")]
    y.train  = a[cv != i, "y"] 
    y.test   = a[cv == i, "y"]

    results <- forecastITS(x.train  = x.train, 
                           x.test   = x.test, 
                           y.train  = y.train, 
                           y.test   = y.test,
                           method   = c("rf", "lm"))

    # Predict for future
    pred <- flattenDataITS(data = data[cv == i,], 
                    index = c(0:-15), 
                    WINDOW = window,
                    STEPS = steps,
                    time = "time",
                    covariates_time = c("year", "season"), 
                    covariates_fix = c("X", "cv"),
                    id = "id", outcome = "y")

    x.pred   = pred[, -c("ID", "time", "cv", "y")]

    prediction <- stepcast(models = results$models, 
                           x.test = x.pred,
                           STEPS = steps, 
                           RMSEweights = results$weights)

    colnames(prediction) <- paste0("LEAD", 1:steps)

    test <- cbind(prediction, pred[,c("ID", "time")])
    
    list_predictions <- split(test, test$time)


    flattenPrediction <- function(x){
      colnames(x)[1:steps] <- paste0("PRED", 1:steps + as.numeric(unique(x$time))) 
      return(x)
    }
    list_predictionsT <- lapply(list_predictions, flattenPrediction)
    list_predictionsT <- rbindlist(list_predictionsT, fill = TRUE)
    list_predictionsT <- setcolorder(list_predictionsT, c("ID", "time"))
    
    predicted <- list_predictionsT[,lapply(.SD, mean, na.rm=TRUE),  
                                    by = c("ID")][, -"time"]

    predicted <-melt(predicted, id.vars = c('ID'), variable.name = "time", 
                      value.name = "prediction")

    predicted <- predicted[, time := (gsub("PRED", "", time))]
    predicted <- predicted[, time := as.numeric(time)]
    
    return_list <- list(list_predictions, list_predictionsT, predicted)
    
    forecast[[i]] <- return_list

}

dfPred <- do.call(rbind, lapply(forecast, function(x) x[[3]]))
dfPred[order(time, ID), ]

dfFinal <- left_join(df, dfPred, by = c("id"="ID", "time"))


dfFinal  %>% 
  group_by(time)  %>% 
  summarise(y = mean(y), y.pred = mean(prediction))  %>% 
  ggplot(aes(y = y, x = time)) +
  geom_line() +
  geom_line(aes(y = y.pred, x = time), color = "red") +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_classic()



# Now we need a way to compute the ITE

ite <- function(data, forecast){

  # Aggregate forecast
  forecastITS <- do.call(rbind, lapply(forecast, function(x) x[[2]]))
  
  t <-melt(forecastITS, id.vars = c('ID', "time"), variable.name = "PRED", 
                      value.name = "prediction")
  t <- t[, time := (gsub("PRED", "", PRED))]
  t <- t[, time := as.numeric(time)]
  t <- t[, c("PRED"):= NULL]
  t <- na.omit(t)
  t$pred <- 1

  m <- left_join(df, t, by =  c("id"="ID", "time"), multiple = "all") 
  m <- na.omit(m)

  m <- m[, ite := prediction - y]
  m <- m[time > -1, ]

  iteM <- cbind(
    m[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("id", "time", "X")],
    "sd"   = m[, lapply(.SD, "sd" = sd), .SDcols = "ite", by = c("id", "time", "X")][, "ite"]
  )

  return(list(ites = iteM))
  }

# One possible aggregation: per time period
  iteMT <- iteM[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("time")]

# Per time and per control group
iteMCT <- iteM[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("time", "X")]

ggplot(iteMT, aes(x = time, y = ite)) +
  geom_bar(stat= "identity")

ggplot(iteMCT, aes(x = time, y = ite, fill = as.factor(X))) +
  geom_bar(stat= "identity", position = "dodge")

# Total ite
mean(iteM$ite); sd(iteM$ite)  


pacman::p_load(fable, feasts, tsibble, tsibbledata)

install.packages("tsibble")
install.packages("forecast")
library(tsibble)
library(forecast)
fit <- nnetar(tsibble(df, key = id, index = time), lambda=0)

test <- df[, lapply(.SD, mean), .SDcols = c("y"), 
            by = "time"]
test <- df[, .(time, id, y)]

fit <- df
fit <- fit[, date := make_yearmonth(year = year, month = season)]
fit <- tsibble(df, key = id , index = date)  

library(tsibbledata)
library(lubridate)

fit
model(fit, fit = TSLM(y ~ trend() + season()))  %>% 
  forecast(h = 7)


fit <- ts.train %>%
  model(
    ets = ETS(y_),
    arima = ARIMA(y_ ~ pdq(1, 0, 0) + PDQ(1, 1, 0)),
    arima_auto = ARIMA(y_, stepwise = FALSE, approx = FALSE),
    croston = CROSTON(y_),
    lm = TSLM(y_ ~ trend() + season())
  ) %>%
  mutate(mixed = (ets + arima + arima_auto + croston + lm) / 5) # creates a combined model using the averages of all individual models

# Check white noise test of residuals
# dof not correct for all
augment(fit) %>%
  features(.innov, ljung_box, lag = 50, d
