#Assumptions
##assets can be freely traded at any trading day (i.e. there are no restrictions); and
##there are no transaction costs when we trade assets.

#Prices of Assets A and B 
A <- ts(c(10, 12, 11, 15, 13, 14, 18, 20, 17, 18) )
B <- ts(c(11, 9, 12, 13.5, 12.5, 13, 20, 19, 19.5, 19) )
ts.plot(A, B, ylab="price", xlab="trading day", lty=1:2, col=1:2)
points(A, pch=20)
points(B, pch=17,col=2)
legend("topleft",
legend = c("Asset A", "Asset B"),
pch = c(20,17),
col = 1:2,
inset = c(0.1, 0.1))
#hypothesised prices of two assets A and B, over 10 trading days. We observe that the two assets seem to follow each other quite closely.

#The annual number of telephone calls in Belgium, spanning 24 years (from 1950 to 1973)
library(MASS)
y <- ts(phones$calls, start=c(1950), frequency=1)
ts.plot(y, ylab="number of calls (in millions)", xlab="year")
points(y, pch=20)
#This data set suggests a slow (or weak) upward trend from 1950 until 1963, followed by
#an abrupt jump which was peaked in 1969 and that was followed by a drop in 1970. Questions of interest here
#would be to try to understand and maybe forecast the jump. Forecasting the number of annual telephone
#calls can help telecom companies to design and plan of services they provide

#Time plot : with consecutive points joined by straight lines to show the relation to time better. 
ts.plot(y, main=expression("Title of the plot"), xlab="time", ylab="series y")

#Stationary time series : the values of the series will change from one point of time to another, but the mean and variance stay the same.
#50 simulated observations from a stationary time series
y.static <- rnorm(50, mean=2, sd=2)
ts.plot(y.static, ylab="observations", xlab="time")
points(y.static, pch=20)

#Histogram and box plot of a stationary time series yt
par(mfrow=c(1,2))
hist(y.static, main=expression("Histogram of y"), xlab="y")
boxplot(y.static, main=expression("Box-plot of y"))

#Trend time series 
# it follows a linear trend (the straight line) plus noise. In other words if we remove the noise from yt we should be able to recover (approximately) the straight line or trend
y.trend <- 2 + 0.8*1:50 + rnorm(50, mean=0, sd=10)
ts.plot(y.trend, ylab="observations", xlab="time")
points(y.trend, pch=20)
lines(2 + 0.8*1:50, col=2)
points(2 + 0.8*1:50, col=2, pch=20, lty=2)

#Quadratic trend time series
y.quadratic <- 2 + 0.8*1:50 -0.4*((1:50)**2) + rnorm(50, mean=0, sd=1)
ts.plot(y.quadratic, ylab="observations", xlab="time")
points(y.trend, pch=20)
lines(2 + 0.8*1:50, col=2)
points(2 + 0.8*1:50, col=2, pch=20, lty=2)
