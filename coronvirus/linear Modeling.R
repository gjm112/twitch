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
plot(as.Date("2020-01-22") + 38:54,y[38:54],ylab = "Confirmed Cases", xlab = "Date", main = "US Confirmed Cases of Corona Virus")

plot(as.Date("2020-01-22") + 1:54,log(y),ylab = "Confirmed Cases (log scale)", xlab = "Date", main = "US Confirmed Cases of Corona Virus")
plot(as.Date("2020-01-22") + 38:54,log(y[38:54]),ylab = "Confirmed Cases (log scale)", xlab = "Date", main = "US Confirmed Cases of Corona Virus")


#Model this as log linear
dat <- data.frame(x = 1:17, y = y[38:54])
a <- lm(log(y) ~ x, data = dat)
summary(a)
#model log(y) = beta0 + beta1*X
# y = exp(beta0 + beta1*X) = exp(beta0)*exp(beta1*X)

pred <- exp(predict(a, newdata = data.frame(x = 18:60), interval = "predict"))
exp(0.340881)

#Now plot it only through April
plot(as.Date("2020-01-22") + 38:54,y[38:54], xlim = c(as.Date("2020-01-22") + 38,as.Date("2020-01-22") + 70), ylim = c(0, 100000000), yaxt = 'n',xlab = "Date", ylab = "MILLIONS of Confirmed Cases", type = "l", main = "US Confirmed Cases of Corona Virus", sub = "Projection in blue with interval in red")
points(as.Date("2020-01-22") + 38:54,y[38:54], xlim = c(as.Date("2020-01-22") + 38,as.Date("2020-01-22") + 100), cex = 0.5, pch = 16)
axis(2, at = c(1000000,20000000,100000000), labels = c("1","20","100"))

points(as.Date("2020-01-22") + 55:(55+nrow(pred)-1), pred[,1], type = "l", col = "blue")
points(as.Date("2020-01-22") + 55:(55+nrow(pred)-1), pred[,2], type = "l", col = "red")
points(as.Date("2020-01-22") + 55:(55+nrow(pred)-1), pred[,3], type = "l", col = "red")


#Now plot it through April
plot(as.Date("2020-01-22") + 38:54,y[38:54], xlim = c(as.Date("2020-01-22") + 38,as.Date("2020-01-22") + 100), ylim = c(0, 100000000), yaxt = 'n',xlab = "Date", ylab = "MILLIONS of Confirmed Cases", type = "l",main = "US Confirmed Cases of Corona Virus", sub = "Projection in blue with interval in red")
points(as.Date("2020-01-22") + 38:54,y[38:54], xlim = c(as.Date("2020-01-22") + 38,as.Date("2020-01-22") + 100), cex = 0.5, pch = 16)
axis(2, at = c(1000000,20000000,100000000), labels = c("1","20","100"))

points(as.Date("2020-01-22") + 55:(55+nrow(pred)-1), pred[,1], type = "l", col = "blue")
points(as.Date("2020-01-22") + 55:(55+nrow(pred)-1), pred[,2], type = "l", col = "red")
points(as.Date("2020-01-22") + 55:(55+nrow(pred)-1), pred[,3], type = "l", col = "red")


April 1 is 16
April 15 is 30

March 1: 6
March 15: 772
April 1: pred[16,1] 1007790
April 15: pred[30,1] 119116279




diff(colSums(confirmed_US[,-c(1:4)]))



library(changepoint)
y <- diff(colSums(confirmed_US[,-c(1:4)]))
results <- cpt.mean(y,method="AMOC")
#results <- cpt.mean(y,method="PELT")
cpts(results)

param.est(results)

plot(results,cpt.col="blue",xlab="Index",cpt.width=4)







#Deaths 
deaths_US <- subset(deaths, deaths$Country.Region == "US") 

#Convert state to character
deaths_US$Province.State <- as.character(deaths_US$Province.State)

plot(colSums(deaths_US[,-c(1:4)]))





#
#confirmed_US_states <- confirmed_US[-grep("[,]",confirmed_US$Province.State),]

#Some exploratory stuff
for (i in 1:nrow(confirmed_US_states)){print(i)
plot(as.Date("2020-01-22") + 1:54,confirmed_US_states[i,-c(1:4)], main = confirmed_US_states$Province.State[i])
}



#Let's get US totals. 
apply(confirmed_US_states[,-c(1:4)], 2, sum)




             


