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

