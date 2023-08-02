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
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/ensembleLearnerITS.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/rmse.R")
source("C:/Users/ASallin/OneDrive - SWICA Krankenversicherung AG/its/stepcastITS.R")

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

    x.train  = a[cv != i, -c("ID", "time", "cv", "y")]
    x.test   = a[cv == i, -c("ID", "time", "cv", "y")]
    y.train  = a[cv != i, "y"] 
    y.test   = a[cv == i, "y"]

    results <- forecastITS(x.train  = x.train, 
                           x.test   = x.test, 
                           y.train  = y.train, 
                           y.test   = y.test,
                           method   = c("lm", "rf"))

    prediction <- stepcast(models = results$models, x.test = x.test,
                              STEPS = 5L, RMSEweights = results$weights)

    colnames(prediction) <- paste0("LEAD", 1:STEPS)

    forecast[[i]] <- prediction

}

b <- split(a, a$cv)

for (i in 1:K){
  b[[i]] <- cbind(b[[i]], forecast[[i]])
}
b <- do.call(rbind, b)


df


for (i in 1:K){
cbind(a[a$cv == 1,], forecast[[i]])

do.call(rbind, forecast )

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


data  %>% 
  group_by(time, pred)  %>% 
  summarise(y = mean(y))  %>% 
  ggplot(aes(y = y, x = time, color = as.factor(pred))) +
  geom_line() +
  geom_point() +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_classic()
# Now, we set the y hat as new outcome and conduct multi-step prediction
