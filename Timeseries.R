# ------- Additive and Multiplicative Decomposition -------
# read in data from csv file
data <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/FABA-L2-Notes/iphonesales.csv", header=TRUE)			

#save as time series data
ipts <- ts(data$Sales,start=c(2007, 3), end=c(2016, 4), frequency=4)

# plot the time series data	
plot(ipts)

# Use additive decomposition
de <- decompose(ipts, type="additive")	

# Plot the decomposition
plot(de)

# plot data with trend and seasonal components
plot(ipts)
lines(de$trend, col=2)
lines(de$seasonal, col=3)

# using multiplicative
de <- decompose(ipts, type="multiplicative")				
plot(de)

# Loess Decomposition
lo <- stl(ipts, s.window="periodic")				
plot(lo)
lo <- stl(ipts, s.window=1)						
plot(lo)

# log of ts data for multiplicative
lipts <- log(ipts)								
lo <- stl(lipts, s.window="periodic")
lo$time.series
newlo = exp(lo$time.series)
newlo									

# ------- Simple Moving Averages ------- 
install.packages("forecast")
library("forecast")
plot(ipts, ylab="iPhone Sales (million units)")

# Simple Moving Average with n = 3
sma <- ma(ipts, order=3)						

# Check calculation of the first value
mean(ipts[1:3])		

lines(sma, col=2)

# Double Moving Average (use this to smooth the data further!)
sma2 <- ma(sma, order=3)				
lines(sma2, col=3)

# ------- Weighted Moving Averages ------- 
# WMA is not supported in the forecast package so will use TTR package
install.packages("TTR")
library("TTR")

plot(ipts, ylab="iPhone Sales (million units)")

# WMA is similar to an EMA, but with linear weighting if the length of w is equal to n. If the length of w is equal to the length of x, the WMA will use the values of w as weights.”
# Weighted Moving Average with n = 4 and w for t
wma <- WMA(ipts, n=2, w = 1:38)		
lines(wma, col="red")
wma <- WMA(ipts, n=4, w=(1:38)^2)
lines(wma, col=3)
wma <- WMA(ipts, n=4, w=(1:38)^10)
lines(wma, col=4)

# Weighted Moving Average with n = 4 and w for n
wma <- WMA(ipts, n=4, w=1:4)			
lines(wma, col=5)
wma <- WMA(ipts, n=4, w=(1:4)^10)
lines(wma, col=6)

# ------- Simple Forecasting Techniques -------
# average method
ifit1 <- meanf(ipts, h=4)				
plot(ifit1, plot.conf=FALSE, ylab="Apple iPhone Sales (million units)")

# naïve method
ifit2 <- naive(ipts, h=4)	
lines(ifit2$mean,col=2)

# seasonal naïve method
ifit3 <- snaive(ipts, h=4)				
lines(ifit3$mean,col=3)

# drift method
ifit4 <- rwf(ipts,drift=TRUE,h=4)			
lines(ifit4$mean,col=5)

# add legend
legend("topleft",lty=1,col=c(4,2,3,5),legend=c("Mean method","Naive method","Seasonal naive method", "Drift method"))			
