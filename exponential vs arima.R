
#In this exercise we compare the accuracy of using exponential smoothing vs arima

emissions <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/FABA-S3 (3)/emissions.csv", header = TRUE)

emts <- ts(emissions$emissions[1:52], start = c(1959,1), end = c(2010,1), frequency = 1)

psales <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/FABA-S3 (3)/propertysales.csv", header = TRUE)

psalests <- ts(psales$Total_Sales[1:259], start = c(1995, 1), end = c(2016,7), frequency = 12)

prius <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/FABA-S3 (3)/prius.csv", header = TRUE)

priusts <- ts(prius$Num[1:25], start=c(2008,3), end = c(2014,3), frequency = 4)

#Decompose the time series data to help understand better the data sets .Cannot decompose the emmissions data because it is yearly and therefore 
#does not have any seasonality

plot(priusts)

priusde <- decompose(priusts, type="additive")

plot(priusde)

#Decompose the property sales data

plot(psalests)

psalestsde <- decompose(psalests, type="additive")

plot(psalestsde)

#Use simple forecasting to forecast the times series : mean, naive, seasonal naive and drift

priusf <- meanf(priusts, h = 4)

plot(priusf, ylab = "Simple Forecast for Prius using mean")

priusn <- naive(priusts, h = 4)

lines(priusn$mean, col=2)

priusn2 <- snaive(priusts, h=4)

lines(priusn2$mean, col = 3)

priusdwf <- rwf(priusts, h = 4, drift = TRUE)

lines(priusdwf$mean, col=5)

legend("topleft", lty = 1, col = c(4,2,3,5), legend = c("Mean Method", "Naive", "Seasonal Naive", "Drift Method") )

#Simple Forecasting for Property Sales

psalesf <- meanf(psalests, h = 24)

plot(psalesf, ylab = "Simple Forecasting for Property Sales")

psalesn <- naive(psalests, h= 24)

lines(psalesn$mean, col=2)

psalesns <- snaive(psalests, h = 24)

lines(psalesns$mean, col = 3)

psalesdwf <- rwf(psalests, h=24, drift=TRUE)

lines(psalesdwf$mean, col=5)

legend("topleft",lty = 1, col = c(4,2,3,5), legend = c("Mean Method", "Naive Method", "Seasonal Naive", "Drift"))


#Simple Forecasting for Emmissions

emtsf <- meanf(emts, h=4)

plot(emtsf, ylab = "Simple Forecasting for Emissions")

emtsn <- naive(emts, h=4)

lines(emtsn$mean, col=2)

emtsns <- snaive(emts, h=4)

lines(emtsns$mean, col=3)

emtsdwf <- rwf(emts, h=4, drift=TRUE)

lines(emtsdwf$mean, col=5)

legend("topleft", lty = 1, col=c(4,2,3,5), legend = c("Mean Method","Naive", "Seasonal Naive","Drift Method"))

#Use Exponential Smoothing to forecast the time series data

#Emissions

emtsfore <- ets(emts, model = "AZN")

plot(forecast(emtsfore))

#Forecast Property Sales data 

psalesfore <- ets(psalests, model = "AZZ")

plot(forecast(psalesfore))

#Simple forecasting for Prius

priusfore <- ets(priusts, model = "ZZZ")

plot(forecast(priusfore))

#Use ARIMA to forecast prius, property sales and emissions

priusarima <- auto.arima(priusts)

plot(forecast(priusarima, h=4))

#Plot Arima for Property sales

psalesarima <- auto.arima(psalests)

plot(forecast(psalesarima, h=24))


#Plot arima forecast for emissions

emissionsarima <- auto.arima(emts)

plot(forecast(emissionsarima, h = 12))

#Evaluate your forecasts using the forecasting accuracy measures and test data

#Emisssions
em <- ts(emissions$emissions[53:56], start = c(2011,1), end = c(2014,1), frequency = 1)

fit <- ets(emts, model = "ZZZ", n=4)
fit2 <- ets(emts, model = "AAN", n=4)

plot(forecast(fit))
lines(em)

plot(forecast(fit2))
lines(em)

accuracy(forecast(fit), em)
accuracy(forecast(fit2), em)

#Property Sales

ps <- ts(psales$Total_Sales[260:263], start = c(2016,8), end = c(2016,11), frequency = 12)

fit3 <- ets(psalests, model = "AAN")
fit4 <- ets(psalests, model = "ZZZ")

plot(forecast(fit3))
lines(ps)

plot(forecast(fit4))
lines(ps)

accuracy(forecast(fit3), ps)
accuracy(forecast(fit4), ps)

#Prius

pr <- ts(prius$Num[26:29], start = c(2014,4), end = c(2015,3), frequency = 4)

fit5 <- ets(priusts, model = "ZZZ")
fit6 <- ets(priusts, model = "ANN")

plot(forecast(fit5))
lines(pr)

plot(forecast(fit6))
lines(pr)

accuracy(forecast(fit5), pr)
accuracy(forecast(fit6), pr)

