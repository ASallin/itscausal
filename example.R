set.seed(281929)

library(ranger)
library(biglasso)
library(bigmemory)
library(purrr)

source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/simulation.R")
data = df
WINDOW = 12L
covariates_time = c("year", "season")
covariates_fix = c("X")
outcome = "y"
id = "id"
time = "time"
INDEX = 0L
STEPS = 5L

source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/flattenDataITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/crossValidateITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/lm_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/lasso_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/ranger_fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/fit.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/ensembleLearnerITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/rmse.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/stepcastITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/forecastITS.R")

#' TO DO
#' 1) So far, I measure accuracy for one period after ("one-step forecast"). However, it could 
#' be more relevant to test for accuracy N periods after ("multi-step forecast"). See 
#' https://otexts.com/fpp2/accuracy.html for more details
#' Maybe add a t + 4 y in the test session and check RMSE. 
#' 2) Add some summary on the RMSE in the output of the function
#' 3) 

fore <- forecastITS(data, time = "time", INDEX = 0L, WINDOW = 12L, covariates_time = c("year", "season"), 
            covariates_fix = c("X"), key = "id", y = "y", method = c("lm", "rf"), 
            K = 5)

dfFinal <- fore$out

dfFinal  %>% 
  right_join(data)  %>% 
  group_by(time)  %>% 
  summarise(y = mean(y), y.pred = mean(y_hat))  %>% 
  ggplot(aes(y = y, x = time)) +
  geom_line() +
  geom_line(aes(y = y.pred, x = time), color = "red") +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_classic()



# Now we need a way to compute the ITE
forecast.object <- fore

ite <- function(data, forecast.object){

  # Aggregate forecast
  fit <- do.call(rbind, lapply(forecast.object$forecast, function(x) x[[2]]))
  
  t <-melt(fit, id.vars = c('ID', "time"), variable.name = "PRED", 
                      value.name = "prediction")
  t <- t[, time := (gsub("PRED", "", PRED))]
  t <- t[, time := as.numeric(time)]
  t <- t[, c("PRED"):= NULL]
  t <- na.omit(t)
  t$pred <- 1

  m <- left_join(data, t, by =  c("id"="ID", "time"), multiple = "all") 
  m <- na.omit(m)

  m <- m[, ite := prediction - y]
  m <- m[time > -1, ]

  iteM <- cbind(
    m[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("id", "time", "X")],
    "sd"   = m[, lapply(.SD, "sd" = sd), .SDcols = "ite", by = c("id", "time", "X")][, "ite"]
  )

  return(list(ites = iteM))
  }

iteM <- ite(data = df, forecast = forecast)[[1]]

# One possible aggregation: per time period
iteMT <- iteM[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("time")]
iteM1 <- iteM[time == 1, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("time")]
ate1
iteM5 <- mean(iteM[time %in% c(1:4), ite])
ate5

# Per time and per control group
iteMCT <- iteM[, lapply(.SD, "mean" = mean), .SDcols = "ite", by = c("time", "X")]

ggplot(iteMT, aes(x = time, y = ite)) +
  geom_bar(stat= "identity")

ggplot(iteMCT, aes(x = time, y = ite, fill = as.factor(X))) +
  geom_bar(stat= "identity", position = "dodge")

# Total ite
mean(iteM$ite); sd(iteM$ite)  




# Fable
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



a <- flattenDataITS(data = data, 
                        index = c(1:10), 
                        WINDOW = window,
                        STEPS = steps,
                        time = "time",
                        covariates_time = c("year", "season"), 
                        covariates_fix = c("X", "cv"),
                        id = "id", 
                        outcome = "y")
    
x.train  = a[cv != 3, -c("time", "cv")][order(ID),]

ts.train <- x.train[, .(year, season, y, ID)]
ts.train <- ts.train[, date := make_yearmonth(year = year, month = season)]
ts.train <- tsibble(ts.train, key = ID , index = date)  

fit.train <- model(ts.train, fit = TSLM(y ~ trend() + season()))  

x.test  = a[cv == 3, -c("time", "cv")][order(ID),]
ts.test <- x.test[, .(year, season, y, ID)]
ts.test <- ts.test[, date := make_yearmonth(year = year, month = season)]
ts.test <- tsibble(ts.test, key = ID , index = date)  


forecast(fit.train, new_data = ts.test)
         h = 7)




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
