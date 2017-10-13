#Conduct time-series analysis on microsoft stock market data

microsoft <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/msft.csv", header = TRUE)

#Use zoo() function from zoo package because ts() is not good at handling daily data. 
#We will keep this data in a data frame instead of turning it into a time series

#First use a 10 day simple moving average

#install.packages("zoo")

library(zoo)

s10 <- SMA(microsoft$Close, n= 10)

#Plot the data and view the smoothed line

plot(microsoft$Close)

lines(s10, col=2)

#Looking at a 10-day weighted average

w10 <- WMA(microsoft$Close, n = 10, w = (1:10)^10)

#Plot the data plot and the smoothing line

plot(microsoft$Close)

lines(w10, col=3)

#Use BBands() to show 3 lines that indicate the strength and direction of a trend. 
#The middle line is a simple moving average set at 20 day line while the upper, 
#lower bands are the +,- standard deviation. 

bb <- BBands(microsoft$Close, sd =2)

bb

#Produces values for up = upper band, mavg = middle band, dn = lower band. 

#Add bb to the microsoft data frame and plot the outputs of bband()

microsoft <- data.frame(microsoft,bb)

plot(microsoft$Close)

lines(microsoft$Close, col = 2)

lines(microsoft$up, col=3)

lines(microsoft$dn, col=4)

lines(microsoft$mavg, col=5)

#bbands allows you to set the type of moving average method you wish to use. 

bb <- BBands(microsoft$Close, n=20, sd=2, maType = WMA, w = (1:20)^5)

bb

#Add bb to microsoft data frame and plot the results


microsoft <- data.frame(microsoft, bb)

plot(microsoft$Close)

lines(microsoft$dn, col= 2)

lines(microsoft$up, col=3)

lines(microsoft$mavg, col=4)

lines(microsoft$Close, col=5)



