confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")



#Model everyone
y <- (colSums(confirmed[,-c(1:4)]))
names(y) <- names(confirmed[,-c(1:4)])
plot(y)
plot(log(y))

#Starting on March 1.
dat <- data.frame(x = 1:(length(y)-39), y = y[40:length(y)])
a <- lm(log(y) ~ x, data = dat)
summary(a)
plot(as.Date("2020-01-22") + 1:56, log(y),ylab = "Confirmed Cases", xlab = "Date", main = "US Confirmed Cases of Corona Virus", xlim = c(as.Date("2020-03-01"),as.Date("2020-03-17")))

model_list <- list()
dat_list_real <- dat_list_pred <- list()
millions <- list()
#Pull out only one country
for (i in c("Italy","US","Spain", "France")){
confirmed_country <- subset(confirmed, confirmed$Country.Region == i) 
y <- colSums(confirmed_country[,-c(1:4)])
names(y) <- names(confirmed_country[,-c(1:4)])

dat_list_real[[i]] <- dat <- data.frame(date = as.Date("2020-03-01") + 0:(length(y)-39-1),
                                    t = 1:(length(y)-39), 
                                   y = y[40:length(y)], 
                                   country = i)

model_list[[i]] <- a <- lm(log(y) ~ t, data = dat)


pred <- data.frame(t = (length(y)-39 + 1):(length(y)-39 + 28))


dat_list_pred[[i]] <- data.frame(date = as.Date("2020-03-01") + 0:(length(y)-39-1 +28),
                                   t = 1:(length(y)-39 + 28), 
                                   y = c(y[40:length(y)],exp(predict(a,pred))), 
                                   country = i, 
                                 proj = c(rep("real",length(y[40:length(y)])),rep("projected",28)))


#Time to hit 1 million
millions[[i]] <- data.frame(y = c(10000,100000,1000000,10000000),
             t = ceiling(c((log(10000) - a$coefficients[1])/a$coefficients[2],(log(100000) - a$coefficients[1])/a$coefficients[2],
(log(1000000) - a$coefficients[1])/a$coefficients[2],
(log(10000000) - a$coefficients[1])/a$coefficients[2])))

millions[[i]]$date <- (as.Date("2020-03-01") + 0:100)[millions[[i]]$t]


}

dat <- do.call(rbind, dat_list_pred)
library(ggplot2)
ggplot(aes(x = date, y = y, colour = country, shape = proj), data = dat) + scale_y_continuous(trans='log10') + geom_line() + geom_point() + geom_vline(xintercept = as.Date("2020-03-18")) + ylab("Confirmed Cases") + xlab("Date") 

dat <- do.call(rbind, dat_list_pred)
library(ggplot2)
ggplot(aes(x = date, y = y, colour = country, shape = proj), data = dat)  + geom_line() + geom_point() + geom_vline(xintercept = as.Date("2020-03-18")) + ylab("Confirmed Cases") + xlab("Date") + ylim(0,25000000)
