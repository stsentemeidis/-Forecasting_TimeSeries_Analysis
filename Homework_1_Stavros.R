####################################################################################
# Before starting, we have to install and activate some R libraries
# in order to use the necessary statistics and time series functions

library(fBasics)  # basic statistics 
library(forecast) # time series functions

data <- read.csv("/Users/stavrostsentemeidis/Google\ Drive/00.\ O-1-7\ -\ Term\ 2/05.\ Forecasting\ Time\ Series/Homework\ 01/Homework\ 1\ -\ Data.csv",header=TRUE,sep=";",dec=",")
summary(data)
series1 = data[,1] [1:300]
series2 = data[,2] [1:300]
series3 = data[,3] [1:300]
series4 = data[,4] [1:300]
series5 = data[,5] [1:2000]
series6 = data[,6]
series7 = data[,7]
# change the number for each series.
y<-series2

# adjust graphic size.
par(mar=c(1,1,1,1))

####################################################################################
# plot 3 basic plots.
par(mfrow=c(3,1))

ts.plot(y)   
acf(y)
pacf(y)

# Calculate some basic metrics.
mean(y)
sd(y)
skewness(y)                     # near to 0 to be normally distributed
kurtosis(y,method=c("moment"))  # near to 3 to be normally distributed


# Test for stationarity (Augmented Dickey Fuller test).
ndiffs(y, alpha=0.05, test=c("adf")) # should be 0


####################################################################################
#Checking for normality.
hist(y,prob=T,ylim=c(0,0.6),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

# Shapiro test for normality.
shapiro.test(y) # should be > 0.05

####################################################################################
# Formal test for White Noise.
Box.test (y, lag = 20, type="Ljung")  # should be > 0.05

####################################################################################
#Testing for STRICT WHITE NOISE.
par(mfrow=c(3,1))
ts.plot(y^2)   
acf(y^2)
pacf(y^2)

Box.test (y^2, lag = 20, type="Ljung")  # should be > 0.05
####################################################################################
# TRANSFORMATION IF NOT STATIONARY AND AGAIN FROM TOP FOR THE TRANSFORMED DATA.
z<-diff(y) # for the mean

z<-log(y)  # for the variance

returns<-diff(log(y)) # for both mean and variance
z<-returns

####################################################################################
# plot 3 basic plots.
par(mfrow=c(3,1))

ts.plot(z)   
acf(z)
pacf(z)

# Calculate some basic metrics.
mean(z)
sd(z)
skewness(z)                     # near to 0 to be normally distributed
kurtosis(z,method=c("moment"))  # near to 3 to be normally distributed


# Test for stationarity (Augmented Dickey Fuller test).
ndiffs(z, alpha=0.05, test=c("adf")) # should be 0


####################################################################################
#Checking for normality.
hist(z,prob=T,ylim=c(0,0.6),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

# Shapiro test for normailty.
shapiro.test(z) # should be > 0.05

####################################################################################
# Formal test for White Noise.
Box.test (z, lag = 20, type="Ljung")  # should be > 0.05

####################################################################################
#Testing for STRICT WHITE NOISE.
par(mfrow=c(3,1))
ts.plot(z^2)   
acf(z^2)
pacf(z^2)

Box.test (z^2, lag = 20, type="Ljung")  # should be > 0.05


# TRANSFORMATION IF NOT STATIONARY AND AGAIN FROM TOP FOR THE TRANSFORMED DATA.
x<-diff(z) # for the mean

z<-log(y)

returns<-diff(log(spot)) # for both mean and variance
z<-returns

####################################################################################
# plot 3 basic plots.
par(mfrow=c(3,1))

ts.plot(x)   
acf(x)
pacf(x)

# Calculate some basic metrics.
mean(x)
sd(x)
skewness(x)                     # near to 0 to be normally distributed
kurtosis(x,method=c("moment"))  # near to 3 to be normally distributed


# Test for stationarity (Augmented Dickey Fuller test).
ndiffs(x, alpha=0.05, test=c("adf")) # should be 0


####################################################################################
#Checking for normality.
hist(x,prob=T,ylim=c(0,0.6),xlim=c(mean(x)-3*sd(x),mean(x)+3*sd(x)),col="red")
lines(density(x),lwd=2)
mu<-mean(x)
sigma<-sd(x)
v<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(v,mu,sigma)
lines(v,yy,lwd=2,col="blue")

# Shapiro test for normailty.
shapiro.test(x) # should be > 0.05

####################################################################################
# Formal test for White Noise.
Box.test (x, lag = 20, type="Ljung")  # should be > 0.05

####################################################################################
#Testing for STRICT WHITE NOISE.
par(mfrow=c(3,1))
ts.plot(x^2)   
acf(x^2)
pacf(z^2)

Box.test (x^2, lag = 20, type="Ljung")  # should be > 0.05
