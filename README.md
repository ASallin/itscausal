
<!-- README.md is generated from README.Rmd. Please edit that file -->

# itscausal

<!-- badges: start -->
<!-- badges: end -->

Welcome to the package page of itscausal. The goal of itscausal is to
flexibly estimate interrupted time series With causal machine learning.

## Installation

You can install the development version of itscausal from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ASallin/itscausal", force = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(itscausal)
library(data.table)
library(dplyr)
library(ggplot2)

set.seed(234598)

# Simulate a time series

# Generate simulated dataset
n.time <- 100 # Number of time points
intervention <- round(0.8*n.time) # Time point of intervention
n.id <- 60 # Number of unique individuals
constant <- 1
order <- 12  # Replace p with the desired order of the autocorrelation
ar_params <- c(0.5, 0.3, 0.001, 0, 0, -.050, -.020, 0, 0, 0, 0.03, 0.15)  # Replace ar1, ar2, ... with the autocorrelation parameters
year.effect <- rnorm(n.time%/%12 + 1, 0, 0.5) # not growing ;-)
year.effect <- c(rep(year.effect[1:(n.time%/%12)], each = 12), 
                 rep(year.effect[(n.time%%12 + 1)], each = n.time%%12))
season.effect <- rnorm(12, 0, 1)
season.effect <- c(rep(season.effect, n.time%/%12), season.effect[1:(n.time%%12)])
X <- sample(c(0,1), n.id, T)

# Generate the time series
ar_process <- function(n.time, ar_params){
  
  errors <- rnorm(n.time, 0, 1) # Generate the random errors
  simulated_data <- numeric(n.time)
  
  for (i in length(ar_params):n.time) {
    simulated_data[i] <- sum(ar_params * simulated_data[(i - length(ar_params)):(i - 1)]) + errors[i]
  } 
  
  simulated_data
}



# Simulate an ITS
df <- data.frame(
  id = rep(seq(1:n.id), each = n.time),
  X = rep(X, each = n.time),
  year = rep(c(rep(1:(n.time%/%12), each = 12), rep(n.time%/%12+1, n.time%%12)), 
             n.id),
  season = rep(c(rep(1:12, n.time%/%12), (1:12)[1:(n.time%%12)]), 
               n.id),
  id.effect = rep(rnorm(n.id, 0, 10), each = n.time),
  # error.time = arima.sim(list(order = c(1, 12, 2), ar = 0.7, ma = c(1,2)), 
  #                        n = n.time*n.id-12, sd=1),
  year.effect = rep(year.effect, n.id),
  season.effect = rep(season.effect, n.id),
  post = rep(c(rep(0, intervention), rep(1, 0.2*n.time)), n.id),
  time = rep((1:n.time)-intervention, n.id),
  error.ar = unlist(replicate(n.id, ar_process(n.time, ar_params), simplify = F)),
  error = rep(rnorm(n.time, 0, 0.5), n.id)
)

# Program y as a function of time and of post*time
df$y = with(df, 
            constant + 0.075*(time + abs(min(df$time))) 
            - (0.085*post) # effect at the cutoff
            - 0.04*post*(time + abs(min(df$time))) # slope after the cutoff
            + year.effect + season.effect + 0.2*X + error + error.ar)


# Final df
df <- df[, c("id", "X", "year", "season", "post", "time", "y")]
df <- data.table(df)

# Simulate y, its "true" value, and the "true" ate for t = 5 and t = 1
df <- df[, model := constant + 0.075* (time + abs(min(df$time)))]
df <- df[, forecast := constant + 0.075*(time + abs(min(df$time))) 
            -  0.085 - 0.04*(time + abs(min(df$time)))]
df <- df[, ite := ifelse(post == 0, NA, model-forecast)]
ate5 <- mean(df[time < 6 & post == 1, ]$ite)
ate1 <- mean(df[time < 2 & post == 1, ]$ite)

print(ate5)
#> [1] 3.365
print(ate1)
#> [1] 3.285
```

We can visualize the its as follows:

    #> `summarise()` has grouped output by 'time'. You can override using the
    #> `.groups` argument.
    #> Warning: Removed 80 rows containing missing values (`geom_line()`).

<img src="man/figures/README-graph-1.png" width="100%" />

We can reproduce these results using the `itscausal` package.

``` r
data <- df

fore <- forecastITS(data, time = "time", INDEX = 0L, WINDOW = 12L, covariates_time = c("year", "season"), 
            covariates_fix = c("X"), key = "id", y = "y", method = c("lm", "rf"), 
            K = 5)
#> 1...
#> 2...
#> 3...
#> 4...
#> 5...

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
#> Joining with `by = join_by(id, time)`
#> Warning: Removed 76 rows containing missing values (`geom_line()`).
```

<img src="man/figures/README-itscausal-1.png" width="100%" />

``` r
# Now we need a way to compute the ITE
iteM <- iteITS(forecast = fore)
```

## Compute the instantaneous treatment effect for the whole population

``` r
InstAte = ateITS(fore, iteM)
InstATE <- InstAte$InstATE

ggplot(InstATE$pred, aes(x = time, y = ite)) +
  geom_bar(stat= "identity")
```

<img src="man/figures/README-InstATE-1.png" width="100%" />

## Per group

``` r
ate1its <- ateITS(fore, iteM, n.periods = 1)
paste("mean = ", round(ate1its$TATE$pred$ite, 3), "; sd = ", round(ate1its$TATE$sd$sd, 3))
#> [1] "mean =  2.446 ; sd =  1.402"

ate5its <- ateITS(fore, iteM, n.periods = 5)
paste("mean = ", round(ate5its$TATE$pred$ite, 3), "; sd = ", round(ate5its$TATE$sd$sd, 3))
#> [1] "mean =  2.684 ; sd =  1.447"

ate1
#> [1] 3.285
ate5
#> [1] 3.365
```
