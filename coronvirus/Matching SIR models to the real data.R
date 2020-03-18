confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")



#Model everyone
y <- (colSums(confirmed[,-c(1:4)]))
plot(y)

# y <- diff(colSums(confirmed[,-c(1:4)]))
# library(changepoint)
# #results <- cpt.mean(y,method="AMOC")
# results <- cpt.mean(y,method="BinSeg", Q = 3)
# cpts(results)


# param.est(results)
# 
# plot(results,cpt.col="blue",xlab="Index",cpt.width=4)
# plot(results,cpt.col="blue",xlab="Index",cpt.width=4,xaxt= 'n')
# axis(1,at = cpts(results), labels = as.Date("2020-01-21") + cpts(results))
# 



#names(confirmed) <- as.character(c("Province/State","Country/Region","Lat","Long",paste0("date_",as.Date("2020-01-22"):as.Date("2020-03-05"))))

#Pull out only US 
confirmed_US <- subset(confirmed, confirmed$Country.Region == "US") 

confirmed_US <- confirmed_US[-grep("Princess", confirmed_US$Province.State),]


#Convert state to character
confirmed_US$Province.State <- as.character(confirmed_US$Province.State)

y <- colSums(confirmed_US[,-c(1:4)])
plot(as.Date("2020-01-22") + 1:54,y,ylab = "Confirmed Cases", xlab = "Date", main = "US Confirmed Cases of Corona Virus")

real <- data.frame(date = as.Date("2020-01-22") + 1:54,y = y)

library(deSolve)

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}


#R0 <- 2.28
R0 <- 5.772334
R0_vec <- seq(1,5,.5)

for (R0 in R0_vec){print(R0)
N <- 330000000
initial_values <- c(
  S = N,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)



parameters_values <- c(
  beta  = (R0/14)/N, # infectious contact rate (/person/day)
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

#sir_values_1$H <- 0.1*sir_values_1$I
# dat <- data.frame(time = rep(sir_values_1$time, 4), y = c(sir_values_1$S, sir_values_1$I, sir_values_1$R, sir_values_1$H), sir = rep(c("S","I","R","H"),each = length(sir_values_1$time)), date = rep(as.Date("2020-01-21") + 0:TTT, 4))

dat <- data.frame(time = rep(sir_values_1$time, 3), y = c(sir_values_1$S, sir_values_1$I, sir_values_1$R), sir = rep(c("S","I","R"),each = length(sir_values_1$time)), date = rep(as.Date("2020-01-21") + 0:TTT, 3))

library(ggplot2)
g <- ggplot(aes(x = date, y = y, colour = sir), data = dat) + geom_line() + geom_vline(aes(xintercept = as.Date("2020-03-14"))) + geom_hline(aes(yintercept = 924000)) + ggtitle(paste0("R0 = ", R0))
print(g)


library(ggplot2)
g <- ggplot() + geom_line(aes(x = date, y = y, colour = sir), data = dat) + geom_point() + geom_vline(aes(xintercept = as.Date("2020-03-14"))) + geom_hline(aes(yintercept = 924000)) + ggtitle(paste0("R0 = ", R0)) + xlim(as.Date("2020-01-21"),as.Date("2020-04-01")) + ylim(0, 5000) + geom_point(aes(x = date, y = y), data = real)
print(g)
}



