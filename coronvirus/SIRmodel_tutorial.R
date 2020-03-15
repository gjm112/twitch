library(deSolve)

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

N <- 330000000
initial_values <- c(
  S = N,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)



parameters_values <- c(
  beta  = (2.28/14)/N, # infectious contact rate (/person/day)
  gamma =  1/14   # recovery rate (/day)
)

#R0 <- 2.28
#R0 = N*beta/ gamma
#R0*gamma = N*beta
#R0*gamma/N = beta

TTT <- 365
time_values <- seq(0, TTT) 


sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_values_1 <- as.data.frame(sir_values_1)

sir_values_1$H <- 0.1*sir_values_1$I

dat <- data.frame(time = rep(sir_values_1$time, 4), y = c(sir_values_1$S, sir_values_1$I, sir_values_1$R, sir_values_1$H), sir = rep(c("S","I","R","H"),each = length(sir_values_1$time)), date = rep(as.Date("2020-01-21") + 0:TTT, 4))



library(ggplot2)
ggplot(aes(x = date, y = y, colour = sir), data = dat) + geom_line() + geom_vline(aes(xintercept = as.Date("2020-03-14"))) + geom_hline(aes(yintercept = 924000))

library(ggplot2)
ggplot(aes(x = date, y = y, colour = sir), data = subset(dat, sir %in% c("H"))) + geom_line() + geom_vline(aes(xintercept = as.Date("2020-03-14"))) + geom_hline(aes(yintercept = 924000))




plot(0.02 * (diff(sir_values_1$R)))
max(0.02 * (diff(sir_values_1$R)))

max(0.1 * (diff(sir_values_1$R)))

sum(0.02 * (diff(sir_values_1$R)))

#Starting at Jan 21








