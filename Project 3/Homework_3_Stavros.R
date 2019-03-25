# *** HOMEWORK 3 ***
# *** IBEX DATASET***
library(fBasics)
library(forecast) 
setwd("~/Google Drive/00. O-1-7 - Term 2/05. Forecasting Time Series/Homework 03")

data<-read.csv("ibex_data.csv",header=TRUE,sep=";",dec=",")
y<-data[,2]    # TEST SET

#######################################################################################################################
##################### TESTING STATIONARY AND TIME SERIES MODEL  ######################################################

ts.plot(y)
# By plotting our timeseries it is noticed that our data is not stationary both in the mean (not constant) as well as,
# in the variance (not constant either). In order to verify our observation we will use the formal test. Also, as our
# data is about weeks we think that there might me seasonality, so we also use the formal test of seasonal differences.
# In order to do so we set s = 52, as again our data is weekly

s=52
nsdiffs(y,m=s,test=c("ocsb"))        # seasonal differences
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences
# Based on the above tests we see that our model should take into account 1 regular and 0 seasonal difference, 
# in order to deal with the lack of stationarity in our data. We also take the logs, to fix the variance.
# Moving on with our analysis, we need to plot the PACF and the ACF graphs to detect spikes. In order to do so,
# we define the number of lags to be equally to 104, which corresponds to 2 years as our data is weekly once more.

nlags=104    
par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  
# Based on the ACF and PACF we verify the fact that our data is not stationary as the ACF decays slowly. Furthermore,
# we notice that there is a cyclic pattern in our data. Looking at the PACF we can see that only lag 1 is out of limits.
# Let's now take the first difference in our data in order to observe the behaviour after the transformation.

fit_0<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,0,0),period=s)) 
fit_0

ts.plot(fit_0$residuals)

par(mfrow=c(2,1))
acf(fit_0$residuals,nlags)
pacf(fit_0$residuals,nlags)    

ndiffs(fit_0$residuals, alpha=0.05, test=c("adf")) # regular differences?

Box.test(fit_0$residuals,lag=20)
shapiro.test(fit_0$residuals) 

hist(y,prob=T,ylim=c(0,0.1),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy2<-dnorm(x,mu,sigma)
lines(x,yy2,lwd=2,col="blue")
# After taking the first difference of our series, we can see that stationarity has been achieved both in the mean
# and the variance. Furthermore, we observe that our residuals are WN by both the plots, 
# in which every lag is within bounce, but also from the Box-Ljung test. Lastly, our residuals are also normal,
# as the Shapiro Test gives us a p value bigger that 0.05. By combining normality with WN we conclude that our 
# residuals are also GWN, which leads to the inference of SWN. 

# As the data is now clearly White Noise (WN), with no linear or non -linear relationship,
# it is decided that the best model to fit our data is a Random Walk, ARIMA with (0,1,0) parameters. 
# We have deducted this as the data is WN and we only conducted one transformation of the original data. 
# The ARIMA is a generalization of the ARMA as we took the first difference.


#######################################################################################################################
##################### BEST REGRESSION MODEL FOR IBEX INDEX  ######################################################

# First we split the data in train and test (80% - 20%)
trainset <- data

model_0 <-lm(IBEX~.-Week, data=trainset) 

lm_stats_0 <- summary(model_0)
lm_stats_0

# After splitting our data in train and test we created our baseline model with all the variables. The results
# that we were given is an Adjusted R-Squared of 0.946 which is pretty good. However we have not tested yet our
# variables for multicollinearity. In order to do so, we are going to use the VIF (Variance Inflation Factor) method.

# As a general rule, if VIF is larger than 5, then multi collinearity is assumed to be high.
# As a result, each time we are going to calculate the VIF values, remove the biggest one,
# re-do the model until all the explanatory variables have a VIF below 5.
# Handle the above procedure with a WHILE loop.

all_vifs <- car::vif(model_0)

signif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the variable with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove this variable
  myForm <- as.formula(paste("IBEX~ ", paste (signif_all, collapse=" + "), sep=""))  # design the new formula
  selectedMod <- lm(myForm, data=trainset)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}

print(all_vifs)
# As a results of the previous method, we conclude that the variable Long_term_rate has to be removed from
# our model. As a result, we have the following new model with its summary.

model_1 <-lm(IBEX~.-Week-Long_term_rate..., data=trainset) 

lm_stats_1 <- summary(model_1)
lm_stats_1
# Summarizing our final regression model for the IBEX, we observe a lower Adjusted R-Squared error
# (now at 0.8988 compared to the previous 0.946), but also a slightly higher Residual Standard Error, from
# 130.5 to 178.8.

# After treating multicollinearity, it is time to observe the behaviour of our residuals.
resids_multi_IBEX <- model_1$residuals

par(mfrow=c(2,2))
plot(resids_multi_IBEX, type='o', xlab='',ylab='',xaxt='n',lwd=2,pch=19,cex=0.1, main='Model IBEX', col='cornflowerblue'); grid()
hist(resids_multi_IBEX,prob=T,ylim=c(0,0.005),xlim=c(mean(resids_multi_IBEX)-3*sd(resids_multi_IBEX),mean(resids_multi_IBEX)+3*sd(resids_multi_IBEX)),col="cornflowerblue")
lines(density(resids_multi_IBEX),lwd=2)
mu<-mean(resids_multi_IBEX)
sigma<-sd(resids_multi_IBEX)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy2<-dnorm(x,mu,sigma)
lines(x,yy2,lwd=2,col="red")
boxplot(resids_multi_IBEX,main='Boxplot', col='cornflowerblue'); grid()
qqnorm(resids_multi_IBEX, col='cornflowerblue', main='QQ plot',xlab=' '); grid()
dev.off()

par(mfrow=c(2,1))
acf(model_1$residuals,nlags)
pacf(model_1$residuals,nlags)    

ndiffs(model_1$residuals, alpha=0.05, test=c("adf")) # regular differences?

Box.test(model_1$residuals,lag=20)
shapiro.test(model_1$residuals) 

# Based on the above graphs, we can notice that our residuals for the regression model_1 are stationary and this 
# can be verified by the ADF test. Also, they are WN as all but one lag are out of limits and the Box-Ljung test
# gives us a p-value greater than 0.05. Furthermore, by looking at the histogram as well as the Shapiro Test,
# we can conclude to normality, which in coordination with the WN gives us GWN for our residuals.

# However, in case to take into account the lag number 2 (in both ACF and PACF), and remove any structure in 
# the residuals we introduce a model AR(2) or MA(2)
# xreg_matrix <- cbind(trainset$Exchange_rate, trainset$Short_term_rate)
# colnames(xreg_matrix) <- c('Exchange_rate','Short_term_rate')
# model_3_0=arima(trainset$IBEX,order=c(1,1,0),xreg=xreg_matrix,include.mean=F) 
# summary(model_3_0)

#######################################################################################################################
##################### BEST REGRESSION - TIMES SERIES MODEL FOR IBEX INDEX  ######################################################

xreg_matrix <- cbind(trainset$Exchange_rate, trainset$Short_term_rate)
colnames(xreg_matrix) <- c('Exchange_rate','Short_term_rate')
model_3_1=arima(trainset$IBEX,order=c(0,1,0),xreg=xreg_matrix,include.mean=F) 
summary(model_3_1)
# As in the first question we ended up that only the difference is enough for our dataset as it is already WN,
# we still have the same amount of lags. Also, regarding the model from question 2, we started with 3 regressors
# but after dealing with multicollinearity, we ended up with 2 which are the ones we use at this model as well.
# Time to check the residuals of this model as well.

ts.plot(model_3_1$residuals)

par(mfrow=c(2,1))
acf(model_3_1$residuals,nlags)
pacf(model_3_1$residuals,nlags)    

ndiffs(model_3_1$residuals, alpha=0.05, test=c("adf"))

Box.test(model_3_1$residuals,lag=20)
shapiro.test(model_3_1$residuals) 
# After checking our residuals, we can still see that there is some structure in the residuals. In order to deal,
# with the lags out of bounce we start by introducing an MA(3) model by looking at the PACF.
model_3_2a=arima(trainset$IBEX,order=c(0,1,4),xreg=xreg_matrix,include.mean=F) 
summary(model_3_2a)

# retuning
model_3_2 <- arima(trainset$IBEX,order=c(0,1,4),fixed=c(0,0,0,NA,NA,NA),xreg=xreg_matrix,include.mean=F) 
summary(model_3_2)


# Now that all our variables are significant let's move on checking the residuals of our model.
ts.plot(model_3_2$residuals)

par(mfrow=c(2,1))
acf(model_3_2$residuals,nlags)
pacf(model_3_2$residuals,nlags)    

ndiffs(model_3_2$residuals, alpha=0.05, test=c("adf"))

Box.test(model_3_2$residuals,lag=20)
shapiro.test(model_3_2$residuals) 

# Based on the above mentioned plots and tests. We now can conclude that our final combined model's residuals,
# are WN and normal, so GWN. Furthermore, below is mentioned the corresponding equation to the model.
# Y_t= -0.4446*Y_(t-3)+0.3654*Y_(t-2)- 0.9208*Y_(t-1) + 2083.7794*〖Ex_Rte〗_(t-1)-141.7185*〖ShortT_Rte〗_(t-1)  + ε_t

##########################################################################################################
# After checking our residuals, we can still see that there is some structure in the residuals. In order to deal,
# with the lags out of bounce we start by introducing an AR(4) model by looking at the PACF.
model_3_10=arima(trainset$IBEX,order=c(4,1,0),xreg=xreg_matrix,include.mean=F) 
summary(model_3_10)
STATIONARY PLUS NOT WN WE MODEL THE RESIDUALS 
#As not all our variables are significant we retune the model. Remove AR(2)
model_3_20=arima(trainset$IBEX,order=c(4,1,0),fixed=c(0,0,0,NA,NA,NA),xreg=xreg_matrix,include.mean=F) 
summary(model_3_20)

# Now that all our variables are significant let's move on checking the residuals of our model.
ts.plot(model_3_20$residuals)

par(mfrow=c(2,1))
acf(model_3_20$residuals,nlags)
pacf(model_3_20$residuals,nlags)    

ndiffs(model_3_20$residuals, alpha=0.05, test=c("adf"))

Box.test(model_3_20$residuals,lag=20)
shapiro.test(model_3_20$residuals) 
# Based on the above mentioned plots and tests. We now can conclude that our final combined model's residuals,
# are WN and normal, so GWN. Furthermore, below is mentioned the corresponding equation to the model.
# Y_t= - 0.6960*Y_(t-1) + 2194.9913*〖Ex_Rte〗_(t-1) - 140.9813*〖ShortT_Rte〗_(t-1)  + ε_t

#######################################################################################################################
##################### COMPARING MODELS ######################################################
# In order to choose the best model to explain the IBEX variable, we need to look at the 
# estimate of the residual variance of each model. In the picture below, 
# the output of the according functions R clearly indicate Model 3 as the one with the lowest 
# Residual Variance

fit_0$sigma2
summary(model_1)
# (172.1*172.1) = 29618.41
model_3_2$sigma2
model_3_20$sigma2

#######################################################################################################################
##################### ONE STEP AHEAD PREDICTIONS AND INTERVALS ######################################################
# onestep <- fitted(model_3_2)
# onestep
# 
# ts.plot(trainset$IBEX,col="cornflowerblue",ylab="IBEX", 
#         main = "IBEX index over weekly data")
# par(new=TRUE)
# plot(onestep,col="red",ylab="", yaxt='n',type = 'l')
# 
# legend("topleft",
#        c("Real Values","One Step Ahead Predictions"),
#        fill=c("cornflowerblue","red")
# )
# 
# quantile(model_3_2$residuals, probs=c(0.025,0.975)) # 95% confidence interval
# -1.96*sd(model_3_2$residuals)# 95% confidence interval

##################### 
testnew <- as.matrix(trainset[,-c(1,2,5)])
testnew <- cbind(0.781,7.6)
colnames(testnew) <- c('Exchange_rate','Short_term_rate')

model_pred<-predict(model_3_2,n.ahead = 1,newxreg = testnew)
model_pred$pred 
model_pred$se    
ts.plot(model_pred$pred)
forecast(model_3_2, h=1,xreg = testnew)



# par(mfrow=c(2,1))
# ts.plot(trainset$IBEX,col="cornflowerblue",ylab="IBEX", 
#         main = "IBEX Timeseries")
# ts.plot(model_pred$pred,col="red",ylab="IBEX", 
#         main = "IBEX One Horizon Ahead")
# legend("topleft",
#        c("Real Values","One Step Ahead Predictions"),
#        fill=c("cornflowerblue","red")
# )
# ##################### 
# upper <- fitted(model_3_2) + 1.96*sqrt(model_3_2$sigma2)
# lower <- fitted(model_3_2) - 1.96*sqrt(model_3_2$sigma2)
# upper[1]
# lower[1]
