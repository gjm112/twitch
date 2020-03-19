confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")


confirmed_country <- subset(confirmed, confirmed$Country.Region == "US") 
confirmed_y <- colSums(confirmed_country[,-c(1:4)])
names(confirmed_y) <- names(confirmed_country[,-c(1:4)])


#Pull out US Deaths
i <- "US"
deaths_country <- subset(deaths, confirmed$Country.Region == i) 
y_deaths <- colSums(deaths_country[,-c(1:4)])
names(y_deaths) <- names(deaths_country[,-c(1:4)])

confirmed_country <- subset(confirmed, confirmed$Country.Region == i) 
y <- colSums(confirmed_country[,-c(1:4)])
names(y) <- names(confirmed_country[,-c(1:4)])

corona <-  data.frame(date = as.Date("2020-01-22") + 0:(length(y)-1), confirmed = y, deaths = y_deaths, t = 1:length(y))

library(ggplot2)
ggplot(aes(x = date, y = log(deaths)), data = corona) + geom_point()

ggplot(aes(x = date, y = (deaths)), data = corona) + geom_point()



a <- lm(log(deaths) ~ I(t - 40), data = subset(corona, date >= "2020-03-02"))
summary(a)
exp(0.18082)

plot(corona$death/corona$confirmed)
#evidence of more testing being done on less sick people.  

#For a given death rate. 
dr <- 0.01
dr <- c(0.005, 0.01, 0.02, 0.03)
results_list <- list()
for (i in 1:length(dr)){
corona$cases <- corona$deaths/dr[i]

a <- lm(log(cases) ~ date, data = subset(corona, date >= "2020-03-02"))
summary(a)
exp(0.18082)

b <- lm(log(deaths) ~ date, data = subset(corona, date >= "2020-03-02"))
summary(b)
exp(0.18082)

pred <- predict(a, data.frame(date = c(as.Date("2020-03-19")+0:27)))

projected <-  data.frame(date = c(as.Date("2020-03-19")+0:27), confirmed = NA, deaths = exp(pred)*dr[i], t = nrow(corona) + 1:28,cases = exp(pred), death_rate = dr[i], type = "projected")

real <- subset(corona, date >= "2020-03-02")
 real$death_rate <- dr[i]
 real$type = "Actual"
 
 results_list[[i]] <- rbind(real,projected)
}

results <- do.call(rbind,results_list)

ggplot() + geom_point(aes(y = cases, x = date, colour = factor(death_rate)), data = results) +geom_line(aes(y = cases, x = date, colour = factor(death_rate)), data = results) + geom_vline(xintercept = as.Date("2020-03-18")) + scale_y_continuous(trans='log10') + geom_point(aes(x = date, y = confirmed), data = subset(corona, date >= "2020-03-01"))

#predict deaths 
pred_death <- exp(predict(b, data.frame(date = c(as.Date("2020-03-19")+0:27)), interval = "prediction"))

ddd <- data.frame(date = c(as.Date("2020-03-19")+0:27),pred_death = pred_death)

ggplot(aes(x = date, y = pred_death.fit), data = ddd) + geom_line(col = "blue") + geom_point(aes(x = date, y = deaths), data = corona) + geom_line(aes(x = date, y = pred_death.lwr), data = ddd, col = "red") + geom_line(aes(x = date, y = pred_death.upr), data = ddd, col = "red") 

ggplot(aes(x = date, y = pred_death.fit), data = ddd) + geom_line(col = "blue") + geom_point(aes(x = date, y = deaths), data = corona) + geom_line(aes(x = date, y = pred_death.lwr), data = ddd, col = "red") + geom_line(aes(x = date, y = pred_death.upr), data = ddd, col = "red") + scale_y_continuous(trans='log10')