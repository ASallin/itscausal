
# Simulation Menchetti
# n = 2
# timePeriods = 10
# interventionTime = 9

simulateITSMenchetti <- function(n, timePeriods, interventionTime = timePeriods-1, linear = TRUE){

    # Parameters
    time <- rep(c(1:timePeriods), n)
    id   <- rep(1:n, each = timePeriods)

    u_i <- rep(rnorm(n, 0, 0.1), each = timePeriods)
    v_t_1 <- rep(rnorm(timePeriods, 0, 1), n)
    v_t_2 <- rep(rnorm(timePeriods, 0, 0.2), n)
    MVR <- MASS::mvrnorm(n = timePeriods, 
                   mu = c(0,0,0), 
                   Sigma = matrix(c(1,0.5,0.7,0.5,1,0.3,0.7,0.3,1), nrow = 3))
    v_t_345 <- do.call(rbind, 
                        replicate(n, MVR, simplify = F)
    )
    v_t_7 <- rep(rnorm(timePeriods, 0, 0.2), n)
    
    x1 <- 0.2*time + u_i + v_t_1
    x2 <- 0.2*time + u_i + v_t_2
    x3 <- u_i + v_t_345[,1]
    x4 <- u_i + v_t_345[,2]
    x5 <- u_i + v_t_345[,3]
    x6 <- u_i - rep(rnorm(timePeriods, 0, 1), n)
    x7 <- (0.2*time + v_t_1)^2 + u_i + rep(rnorm(timePeriods, 0, 0.2), n)
    x8 <- sample(c(0,1), n*timePeriods, replace = T)
    x9 <- sample(c(1:3), n*timePeriods, replace = T)
    x10 <- x3 * x8
    x11 <- x2 * x9

    beta1 <- beta8 <- beta9 <- 0
    beta2 <- beta6 <- beta10 <- 2
    beta3 <- beta7 <- 1
    beta4 <- 2.5
    beta5 <- 0.1
    beta11 <- 1.5

    df <- cbind(time, id, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
    df <- cbind(df, y = 0)
    df <- as.data.frame(df)

    if(linear){
    for(i in 1:nrow(df)){
        df[i, "y"] <- df[i, "y"]*0.8 + 
                    beta1*df[i, "x1"]+ 
                    beta2*df[i, "x2"] +
                    beta3*df[i, "x3"] +
                    beta4*df[i, "x4"] +
                    beta5*df[i, "x5"] +
                    beta6*df[i, "x6"] +
                    beta7*df[i, "x7"] +
                    beta8*df[i, "x8"] +
                    beta9*df[i, "x9"] +
                    beta10*df[i, "x10"] +
                    beta11*df[i, "x11"] + 
                    rnorm(1, 0, 1)

        if(i != nrow(df)){
            df[i + 1, "y"] <- df[i, "y"]
        } else {break}
    }
    } else {
        for(i in 1:nrow(df)){
        df[i, "y"] <- sin(sin(df[i, "y"]*0.8 + 
                    beta1*df[i, "x1"]+ 
                    beta2*df[i, "x2"] +
                    beta3*df[i, "x3"] +
                    beta4*df[i, "x4"] +
                    beta5*df[i, "x5"] +
                    beta6*df[i, "x6"] +
                    beta7*df[i, "x7"] +
                    beta8*df[i, "x8"] +
                    beta9*df[i, "x9"] +
                    beta10*df[i, "x10"] +
                    beta11*df[i, "x11"])) + 
                    rnorm(1, 0, 1)

        if(i != nrow(df)){
            df[i + 1, "y"] <- df[i, "y"]
        } else {break}
    }
    }

    # Intervention: add 2 sds to interventiono outcome
    int <- as.matrix(rep(sapply(split(df, df$id), function(x) sd(x$y)), each = timePeriods))
    df <- cbind(df, int)
    df[df$time < interventionTime, "int"] <- 0
    
    df$y_obs <- df$y + 2*df$int

    # Compute ATE1
    ate1 <- mean(2*df[df$int !=0, ]$int)

    df$int <- NULL
    row.names(df) <- NULL
    
    return(list(df, ate1))
}


library(itscausal)
library(purrr)
library(tidyr)

dfMap <- crossing(n=c(50, 100, 200),
          timePeriods = c(20, 10, 5),
          interventionTime = c(18, 8, 4),
          linear = c(T, F))  %>% 
          filter(timePeriods - interventionTime < interventionTime,
                 timePeriods > interventionTime)  %>% 
          mutate(df = pmap(list(n, timePeriods, interventionTime, linear),
                            simulateITSMenchetti))  %>% 
          mutate(df = map(df, pluck, 1),
                 ate = map(df, pluck, 2))

# For ate1
dfMap1 <- dfMap  %>% 
    slice(3)  %>% 
    mutate(f = pmap(list(df, n, timePeriods, interventionTime),
                    ~forecastITS(data = ..1, 
                                 time = "time", 
                                 INDEX = ..4, 
                                 WINDOW = as.integer(..4-2L), 
                                 covariates_time = NULL, 
                                 STEPS = 1L,
                                 key = "id",
                                 y = "y"))
                                 )


data <- dfMap[3,]$df[[1]]
INDEX <- 8
timePeriods = 10
STEPS = 1L
WINDOW = INDEX-STEPS-2L
covariates_time = NULL
covariates_fix = NULL
time = "time"
key = "id"
y = "y"
method = c("lm", "rf", "xgboost")
K = 5
CYCLE = WINDOW
FORECASTUNITS = NULL



map(dfMap1$f, pluck, "RMSEweights")

fore <- dfMap1[11,]$f[[1]]
dfAnalysis <- dfMap1[11,]$f[[1]]$data

dfFinal <- fore$out

ITS.plot <- function(object, interruption = NULL){

  interruption <- fore$call$INDEX
  y <- fore$call$y
  
  df <- left_join(fore$data, fore$out)
  df <- na.omit(df)

  plot <- ggplot(df, aes(x = time, y = y_))
  plot <- 
  fore$data
  fore$out
  str(fore)

}
library(ggplot2)

dfFinal$y_hat

dfFinal  %>% 
  right_join(dfAnalysis)  %>% 
  group_by(time)  %>% 
  summarise(y = mean(y), y.pred = mean(y_hat, na.rm = T))  %>% 
  ggplot(aes(y = y, x = time)) +
  geom_line() +
  geom_point(aes(y = y.pred, x = time), color = "red") +
  geom_line(aes(y = y.pred, x = time), color = "red") +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
#   geom_vline(xintercept = 0, color = "red", linetype = 2) +
#   geom_vline(xintercept = 15, color = "red", linetype = 2) +
  theme_classic()


# ITE
forecast.object = fore

iteM <- iteITS(forecast.object = fore)

InstAte = ateITS(fore, iteM)
InstATE <- InstAte$InstATE

ggplot(InstATE$pred, aes(x = time, y = ite)) +
  geom_bar(stat= "identity")

# df <- simulateITSMenchetti(n = 50, timePeriods =6, interventionTime = 5, linear = F)


# data <- df
# INDEX = 5L
# timePeriods = 7
# WINDOW = as.integer(floor(timePeriods*0.66))
# STEPS = as.integer(interventionTime-WINDOW)
# covariates_time = NULL
# covariates_fix = NULL
# time = "time"
# key = "id"
# y = "y"
# method = c("lm", "rf", "xgboost")
# K = 5
# CYCLE = 5L
# FORECASTUNITS = NULL

# f    <- forecastITS(data = df, time = "time", INDEX = 8L, WINDOW = as.integer(floor(10*0.66)), covariates_time = NULL, key = "id", 
#                     y = "y")


simulateITS <- function(n, timePeriods, timeAfter, effectCutOff = NULL, groups = 1){

    # Parameters
    trend <- rnorm(1, 0, 1)

    time <- rep(c(1:timePeriods), n)
    id   <- rep(1:n, each = timePeriods)
    groups <- rep(sample(1:groups, n, replace = T), each = timePeriods)
    month <- time %% 12
    year  <- time %/% 12

    if(is.null(effectCutOff)){
        y    <- time + trend*time + rnorm(50, 0, 1) 
    } else {
        y    <- time + trend*time + rnorm(50, 0, 1) + effectCutOff * (timePeriods > timeAfter)
    }

    df   <- data.frame(id, time, year, y)

}


