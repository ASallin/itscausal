# Set-up
library(ggplot2)
library(dplyr)
library(data.table)


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


df$y = with(df, constant + (0.075*time) - (0.85*post) -0.04*post*time + 
              + year.effect + season.effect + 0.2*X + error + error.ar)


# Final df
df <- df[, c("id", "X", "year", "season", "post", "time", "y")]
df <- data.table(df)




df  %>% 
  group_by(time)  %>% 
  summarise(y = mean(y))  %>% 
  ggplot(aes(y = y, x = time)) +
  geom_line() +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_classic()


segm1 <- lm(y  ~ time + post + post*time,
            data = df) 
summary(segm1)

# With Newey West standard errors
coeftest(segm1, vcov = NeweyWest)



# Old simulation code
# Generate simulated dataset
n.time <- 100 # Number of time points
intervention <- round(0.8*n.time) # Time point of intervention
n.id <- 15 # Number of unique individuals
sds <- 2 
constant <- 1

fe.id <- seq(1:n.id)
fe.coeff <- rnorm(length(fe.id), 1, 0.5)
season.id <- c(rep(1:12, n.time%/%12), (1:12)[1:(n.time%%12)])
season.effect <- rnorm(12, 0, 1)
season.effect <- c(rep(season.effect, n.time%/%12), season.effect[1:(n.time%%12)])
year.id <- c(rep(1:(n.time%/%12), each = 12), rep(n.time%/%12+1, n.time%%12))
year.effect <- rnorm(n.time%/%12 + 1, 0, 0.5) # not growing ;-)
year.effect <- c(rep(year.effect[1:(n.time%/%12)], each = 12), 
                 rep(year.effect[(n.time%%12 + 1)], each = n.time%%12))


# Simulate a diff and diff
df <- data.frame(
  id = rep(fe.id, each = n.time),
  id.effect = rep(rnorm(n.id, 0, 10*sds), each = n.time),
  post = rep(c(rep(0, intervention), rep(1, 0.2*n.time)), length(fe.id)),
  time = rep((1:n.time)-intervention, length(fe.id)),
  error = rep(rnorm(n.time, 0, 0.5), length(fe.id)),
  error.time = arima.sim(list(order = c(1, 12, 2), ar = 0.7, ma = c(1,2)), 
                         n = n.time*n.id-12, sd=1),
  season = rep(season.id, length(fe.id)),
  season.effect = rep(season.effect, length(fe.id)),
  year = rep(year.id, length(fe.id)),
  year.effect = rep(year.effect, length(fe.id))
)


df$y = with(df, constant + (0.075*time) - (0.85*post) -0.04*post*time + 
              season.effect + year.effect + error)

