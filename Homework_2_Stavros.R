# *** HOMEWORK 2 ***
# *** COCA COLA DATASET***
library(fBasics)
library(forecast) 

data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2][1:95]           # TRAINING SET
y_test<-data[,2][96:107]    # TEST SET
#######################################################################################################################
##################### TESTING STATIONARY AND TIME SERIES MODEL  ######################################################

ts.plot(y)
# By plotting our timeseries it is noticed that our data is not stationary both in the mean (not constant) as well as,
# in the variance (not constant either). In order to verify our observation we will use the formal test. Also, as our
# data is about quarters we think that there might me seasonality, so we also use the formal test of seasonal differences.
# In order to do so we set s = 4, as again our data is quarterly.

s=4
nsdiffs(y,m=s,test=c("ocsb"))        # seasonal differences
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences
yy <- log(y)
# Based on the above tests we see that our model should take into account 1 regular and 1 seasonal difference, 
# in order to deal with the lack of stationarity in our data. We also take the logs, to fix the variance.
# Moving on with our analysis, we need to plot the PACF and the ACF graphs to detect spikes. In order to do so,
# we define the number of lags to be equally to 40, which corresponds to 10 years as our data is quarterly once more.

nlags=40     
par(mfrow=c(2,1))
acf(yy,nlags)
pacf(yy,nlags)  
######################################################################################################### 
fit_0<-arima(yy,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit_0

ts.plot(fit_0$residuals)

par(mfrow=c(2,1))
acf(fit_0$residuals,nlags)
pacf(fit_0$residuals,nlags)    

ndiffs(fit_0$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit_0$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit_0$residuals,lag=20)
shapiro.test(fit_0$residuals)  
#######################################################################################################################
# We can start modelling from the non seasonal part cause it seems more "strong" at this point in time.
# In our first fit, we define the number of differences for both to be equal to 1, as already seen before.
# By looking at the PACF (simpler) we are trying to detect an AR model. Based on the graph, we choose to start
# with an AR(3) as 4 corresponds on a seasonal lag, and anything after that will include the 4th one.

fit_1<-arima(yy,order=c(3,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit_1
# By looking at the summary of the model we can see that lag 2 is not significant so we re-tune our model.

fit_2<-arima(yy,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=s))  
fit_2
# Having concluded that our lags are significant at a 95% level, we can now have a look at our model's residuals.

dev.off()
ts.plot(fit_2$residuals)
# By observing the plot we can see that our data is close to stationary (which we assume), with the exception of 
# some outliers. Moving on, we check our residuals for WN in order to determine how good our model is.

par(mfrow=c(2,1))
acf(fit_2$residuals,nlags)
pacf(fit_2$residuals,nlags)
Box.test(fit_2$residuals,lag=40)
shapiro.test(fit_2$residuals)
# Our residuals are not WN, so our job is not done yet.
# Lets move on the the seasonal part of the model.
# By looking at the PACF plot, we can see that lag 16 is nearly out of bounds. So we are adding a SAR(4) in our model.
#######################################################################################################################
fit_3<-arima(yy,order=c(1,1,0),seasonal=list(order=c(4,1,0),period=s))  
fit_3

fit_3a<-arima(yy,order=c(1,1,0),seasonal=list(order=c(3,1,0),period=s))  
fit_3a
# By looking at the summary, we can tell that all our lags are not significant.
# So we re-tune and move on checking our residuals. To do that we keep the first 2 lags of the seasonal part
# and only the first one from the non seasonal, cause when we introduce to the model the seasonal part
# the lags 2 and 3 of the AR model become insignificant.

fit_4<-arima(yy,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s))  
fit_4

dev.off()
ts.plot(fit_4$residuals)

par(mfrow=c(2,1))
acf(fit_4$residuals,nlags)
pacf(fit_4$residuals,nlags)
Box.test(fit_4$residuals,lag=20)
shapiro.test(fit_4$residuals)

quantile(fit_4$residuals, probs=c(0.025,0.975)) # 95% confidence interval
quantile(fit_4$residuals, probs=c(0.1,0.9)) # 80% confidence interval
quantile(fit_4$residuals, probs=c(0.2,0.8)) # 60% confidence interval

# Based on the graph but also on the Box - Ljung Test we can tell that our residuals are WN, which indicates
# that our model is sufficiently good to be considered one of our possible models.
# As a next step, let's apply predictions to our model.
#######################################################################################################################

y.pred_A<-predict(fit_4,n.ahead=12)
y.pred_A$pred 
y.pred_A$se    
ts.plot(y.pred_A$pred)
# After that it is time to undo the logarithmic transformation that was applied to fix the non-stationarity 
# in the variance.

y.pred_AA <- exp(y.pred_A$pred)
y.pred_AA

par(mfrow=c(2,1))
ts.plot(y.pred_A$pred)
ts.plot(y.pred_AA)
########################### 


dev.off()



#######################################################################################################################
##################### TESTING STATIONARY AND MA MODEL  ######################################################

ts.plot(y)
# By plotting our timeseries it is noticed that our data is not stationary both in the mean (not constant) as well as,
# in the variance (not constant either). In order to verify our observation we will use the formal test. Also, as our
# data is about quarters we think that there might me seasonality, so we also use the formal test of seasonal differences.
# In order to do so we set s = 4, as again our data is quarterly.

s=4
nsdiffs(y,m=s,test=c("ocsb"))        # seasonal differences
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences
yy <- log(y)
# Based on the above tests we see that our model should take into account 1 regular and 1 seasonal difference, 
# in order to deal with the lack of stationarity in our data. We also take the logs, to fix the variance.
# Moving on with our analysis, we need to plot the PACF and the ACF graphs to detect spikes. In order to do so,
# we define the number of lags to be equally to 40, which corresponds to 10 years as our data is quarterly once more.

nlags=40     
par(mfrow=c(2,1))
acf(yy,nlags)
pacf(yy,nlags)  

fit_00<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit_00

ts.plot(fit_00$residuals)

par(mfrow=c(2,1))
acf(fit_00$residuals,nlags)
pacf(fit_00$residuals,nlags)    

ndiffs(fit_00$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit_00$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit_00$residuals,lag=20)
shapiro.test(fit_00$residuals)  
#######################################################################################################################
# We can start modelling from the non seasonal part cause it seems more "strong" at this point in time.
# In our first fit, we define the number of differences for both to be equal to 1, as already seen before.
# By looking at the ACF (complex) we are trying to detect an MA model. Based on the graph, we choose to start
# with an MA(3) as 4 corresponds on a seasonal lag, and anything after that will include the 4th one.

fit_10<-arima(yy,order=c(0,1,3),seasonal=list(order=c(0,1,0),period=s))  
fit_10

# Having concluded that our lags are significant at a 95% level, we can now have a look at our model's residuals.

dev.off()
ts.plot(fit_10$residuals)
# By observing the plot we can see that our data is close to stationary (which we assume), with the exception of 
# some outliers. Moving on, we check our residuals for WN in order to determine how good our model is.

par(mfrow=c(2,1))
acf(fit_10$residuals,nlags)
pacf(fit_10$residuals,nlags)
Box.test(fit_10$residuals,lag=20)
shapiro.test(fit_10$residuals)
# Our residuals are WN, but our job is not done yet.
# Lets move on the the seasonal part of the model.
# By looking at the PACF plot, we can see that lag 16 is out of bounds. So we are adding a SMA(4) in our model.
#######################################################################################################################
fit_30<-arima(yy,order=c(0,1,3),seasonal=list(order=c(4,1,0),period=s))  
fit_30
# By looking at the summary, we can tell that all our lags are not significant.
# So we re-tune our model and move on checking our residuals.

fit_40<-arima(yy,order=c(0,1,1),seasonal=list(order=c(3,1,0),period=s))  
fit_40

dev.off()
ts.plot(fit_40$residuals)

par(mfrow=c(2,1))
acf(fit_40$residuals,nlags)
pacf(fit_40$residuals,nlags)
Box.test(fit_40$residuals,lag=20)
shapiro.test(fit_40$residuals)

# quantile(fit_40$residuals, probs=c(0.025,0.975)) # 95% confidence interval
# quantile(fit_40$residuals, probs=c(0.1,0.9)) # 80% confidence interval
# quantile(fit_40$residuals, probs=c(0.2,0.8)) # 60% confidence interval


# Based on the graph but also on the Box - Ljung Test we can tell that our residuals are WN, which indicates
# that our model is sufficiently good to be considered one of our possible models.
# As a next step, let's apply predictions to our model.
#######################################################################################################################

y.pred_B<-predict(fit_40,n.ahead=12)
y.pred_B$pred 
y.pred_B$se    
ts.plot(y.pred_B$pred)
# After that it is time to undo the logarithmic transformation that was applied to fix the non-stationarity 
# in the variance.

y.pred_BB <- exp(y.pred_B$pred)
y.pred_BB

par(mfrow=c(2,1))
ts.plot(y.pred_B$pred)
ts.plot(y.pred_BB)
########################### 


#######################################################################################################################
##################### TESTING STATIONARY AND STARTING WITH SEASONAL PART  ######################################################

ts.plot(y)
# By plotting our timeseries it is noticed that our data is not stationary both in the mean (not constant) as well as,
# in the variance (not constant either). In order to verify our observation we will use the formal test. Also, as our
# data is about quarters we think that there might me seasonality, so we also use the formal test of seasonal differences.
# In order to do so we set s = 4, as again our data is quarterly.

s=4
nsdiffs(y,m=s,test=c("ocsb"))        # seasonal differences
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences
yy <- log(y)
# Based on the above tests we see that our model should take into account 1 regular and 1 seasonal difference, 
# in order to deal with the lack of stationarity in our data. We also take the logs, to fix the variance.
# Moving on with our analysis, we need to plot the PACF and the ACF graphs to detect spikes. In order to do so,
# we define the number of lags to be equally to 40, which corresponds to 10 years as our data is quarterly once more.

nlags=40     
par(mfrow=c(2,1))
acf(yy,nlags)
pacf(yy,nlags)  


fit_000<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit_000

ts.plot(fit_00$residuals)

par(mfrow=c(2,1))
acf(fit_000$residuals,nlags)
pacf(fit_000$residuals,nlags)    

ndiffs(fit_000$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit_000$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit_000$residuals,lag=20)
shapiro.test(fit_000$residuals) 
#######################################################################################################################
# We can start modelling from the  seasonal part, hoping that the non seasonal one will fix after the seasonal is done.
# In our first fit, we define the number of differences for both to be equal to 1, as already seen before.
# By looking at the ACF we are trying to detect an MA model. Based on the graph, we choose to start
# with an SMA(2) as 8 corresponds on the last seasonal lag of limits.

fit_100<-arima(yy,order=c(0,1,0),seasonal=list(order=c(0,1,2),period=s))  
fit_100
# By looking at the summary of the model we can see that lag 1 is the only significant so we re-tune our model.

fit_200<-arima(yy,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s))  
fit_200
# Having concluded that our lags are significant at a 95% level, we can now have a look at our model's residuals.

dev.off()
ts.plot(fit_200$residuals)
# By observing the plot we can see that our data is close to stationary (which we assume), with the exception of 
# some outliers. Moving on, we check our residuals for WN in order to determine how good our model is.

par(mfrow=c(2,1))
acf(fit_200$residuals,nlags)
pacf(fit_200$residuals,nlags)
Box.test(fit_200$residuals,lag=20)
shapiro.test(fit_200$residuals)
# Our residuals are not WN, so our job is not done yet.
# Lets move on the the non seasonal part of the model.
# By looking at the PACF plot, we can see that lag 1 is nearly out of bounds. So we are adding a AR(1) in our model.
#######################################################################################################################
fit_300<-arima(yy,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s))  
fit_300

fit_3000<-arima(yy,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s))  
fit_3000
# By looking at the summary, we can tell that all our lags are  significant.
# So we move on checking our residuals.


dev.off()
ts.plot(fit_300$residuals)

par(mfrow=c(2,1))
acf(fit_300$residuals,nlags)
pacf(fit_300$residuals,nlags)
Box.test(fit_300$residuals,lag=20)
shapiro.test(fit_300$residuals)
##########
dev.off()
ts.plot(fit_3000$residuals)

par(mfrow=c(2,1))
acf(fit_3000$residuals,nlags)
pacf(fit_3000$residuals,nlags)
Box.test(fit_3000$residuals,lag=20)
shapiro.test(fit_3000$residuals)

# quantile(fit_4$residuals, probs=c(0.025,0.975)) # 95% confidence interval
# quantile(fit_4$residuals, probs=c(0.1,0.9)) # 80% confidence interval
# quantile(fit_4$residuals, probs=c(0.2,0.8)) # 60% confidence interval

# Based on the graph but also on the Box - Ljung Test we can tell that our residuals are WN, which indicates
# that our model is sufficiently good to be considered one of our possible models.
# As a next step, let's apply predictions to our model.
#######################################################################################################################

y.pred_C<-predict(fit_300,n.ahead=12)
y.pred_C$pred 
y.pred_C$se    
ts.plot(y.pred_C$pred)
# After that it is time to undo the logarithmic transformation that was applied to fix the non-stationarity 
# in the variance.

y.pred_CC <- exp(y.pred_C$pred)
y.pred_CC

par(mfrow=c(2,1))
ts.plot(y.pred_C$pred)
ts.plot(y.pred_CC)
########################### 
#######################################################################################################################

y.pred_C3000<-predict(fit_3000,n.ahead=12)
y.pred_C3000$pred 
y.pred_C3000$se    
ts.plot(y.pred_C3000$pred)
# After that it is time to undo the logarithmic transformation that was applied to fix the non-stationarity 
# in the variance.

y.pred_CC3000 <- exp(y.pred_C3000$pred)
y.pred_CC3000

par(mfrow=c(2,1))
ts.plot(y.pred_C3000$pred)
ts.plot(y.pred_CC3000)
########################### 


dev.off()


#######################################################################################################################
# CREATE TABLE TO COMPARE PREDICTIONS OF THE 3 MODELS

# Model 1: fit_4<-arima(yy,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s))  
# Model 2: fit_40<-arima(yy,order=c(0,1,1),seasonal=list(order=c(3,1,0),period=s))  
# Model 3: fit_300<-arima(yy,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s))  
# Model 4: fit_3000<-arima(yy,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=s)) 

y_total <- cbind(y.pred_AA,y.pred_BB,y.pred_CC,y.pred_CC3000,y_test)
colnames(y_total) <- c('First Model','Second Model','Third Model','Fourth Model','Test Values')
y_total

plot(y_test, y.pred_AA, type = 'p', col = 'blue', cex = 1, pch=19,
     main="Predictions vs TestSet",
     ylab="Predictions", xlab='TestSet')
points(y_test, y.pred_BB, type = 'p', col = 'red', cex=1, pch=19)
points(y_test, y.pred_CC, type = 'p', col = 'green', cex =1, pch=19)
points(y_test, y.pred_CC3000, type = 'p', col = 'purple', cex=1, pch=19)

legend("topleft",
       c("Model 1","Model 2", "Model 3",'Model 4'),
       fill=c("blue","red",'green', 'purple')
)
abline(a=0, b=1, col = 'grey')

################
accuracy(y.pred_AA, y_test)
accuracy(y.pred_BB, y_test)
accuracy(y.pred_CC, y_test)
accuracy(y.pred_CC3000, y_test)

print(paste0('The MAPE of Model 1 is ',accuracy(y.pred_AA, y_test)[5]))
print(paste0('The MAPE of Model 2 is ',accuracy(y.pred_BB, y_test)[5]))
print(paste0('The MAPE of Model 3 is ',accuracy(y.pred_CC, y_test)[5]))
print(paste0('The MAPE of Model 4 is ',accuracy(y.pred_CC3000, y_test)[5]))




