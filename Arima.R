# Install packages
#install.packages("forecast")
library("forecast")

# Import data
data <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/FABA-L3-Notes (1)/iphonesales.csv", header=TRUE)
# save as time series data
ipts <- ts(data$Sales,start=c(2007, 3), end=c(2016, 4), frequency=4)

# ---------- Simple Exponential Smoothing ----------
# Simple Exponential Smoothing - low alpha
SES <- ses(ipts, alpha=0.1)			
plot(SES)

# Simple Exponential Smoothing - high alpha
SES2 <- ses(ipts, alpha=0.8)			
plot(SES2)

# Simple Exponential Smoothing with alpha calculated for you
SES3 <- ses(ipts)					
summary(SES3)					
plot(SES3)

# Simple Exponential Smoothing using the ets function
se <- ets(ipts, model="ANN")			
plot(forecast(se))

# Multiplicative Holt-Winters' method with multiplicative errors
se <- ets(ipts, model="MAM")	
plot(forecast(se))

# ---------- ARIMA ----------
# ACF and PACF plots
tsdisplay(ipts)								
# first differenced plots
tsdisplay(diff(ipts))							
# second differenced plots
tsdisplay(diff(diff(ipts)))							

# ARIMA (0,1,0)
arm <- arima(ipts,order=c(0,1,0))					
plot(forecast(arm))
tsdisplay(residuals(arm))

# ARIMA (2,1,0)
arm <- arima(ipts,order=c(2,1,0))					
plot(forecast(arm))
tsdisplay(residuals(arm))

# Autofit ARIMA inc seasonal
afit <- auto.arima(ipts)							
# Plot 1yr ARIMA forecast
plot(forecast(afit,h=4))							
# Plot 2yr ARIMA forecast
plot(forecast(afit,h=8))							
# Plot 3yr ARIMA forecast
plot(forecast(afit,h=12))							
afit									
# ACF and PACF plots
tsdisplay(residuals(afit))							

# ---------- Forecast Accuracy ----------
fit1 <- ets(ipts[1:34], model="ANN", n=4)
fit2 <- ets(ipts[1:34], model="ZZZ", n=4)
plot(forecast(fit1))
lines(ipts[1:38])
plot(forecast(fit2))
lines(ipts[1:38])
accuracy(forecast(fit1), ipts[35:38])
accuracy(forecast(fit2), ipts[35:38])
