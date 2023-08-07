rm(list = ls())

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

# Program y as a function of time and of post*time
df$y = with(df, 
            constant + 0.075*(time + abs(min(df$time))) 
            - (0.085*post) # effect at the cutoff
            - 0.04*post*(time + abs(min(df$time))) # slope after the cutoff
            + year.effect + season.effect + 0.2*X + error + error.ar)


# Final df
df <- df[, c("id", "X", "year", "season", "post", "time", "y")]
df <- data.table(df)


df <- df[, model := constant + 0.075* (time + abs(min(df$time)))]
df <- df[, forecast := constant + 0.075*(time + abs(min(df$time))) 
            -  0.085 - 0.04*(time + abs(min(df$time)))]
df <- df[, ite := ifelse(post == 0, NA, model-forecast)]
ate5 <- mean(df[time < 5 & post == 1, ]$ite)
ate1 <- mean(df[time < 2 & post == 1, ]$ite)

print(ate5)
print(ate1)

# Graph
df  %>% 
  group_by(time, post)  %>% 
  summarise(y = mean(y))  %>% 
  ggplot(aes(y = y, x = time)) +
  geom_line() +
  geom_line(aes(x = time, y = constant + 0.075* (time + abs(min(df$time)))),
            col = "red") +
  geom_line(aes(x = time, 
                y = ifelse(time < 1, NA, 
                constant + 0.075* (time + abs(min(df$time)))
                - (0.085*post)
                - 0.04*post*(time + abs(min(df$time))))),
            col = "blue") +
  labs(x = "Time to/from the intervention",
       y = "Average y per time period") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_classic()

# segm1 <- lm(y  ~ time + post + post*time,
#             data = df) 
# summary(segm1)

# # With Newey West standard errors
# coeftest(segm1, vcov = NeweyWest)



