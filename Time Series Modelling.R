#Install Packages 

install.packages("Metrics")
library(Metrics)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("forecast")
library(forecast)

###############################################QUESTION1-TICKETS###################################
summary(Tixdf)
View(Tixdf)
plot(Tixdf$Time,Tixdf$Tickets)
lines(Tixdf$Time, Tixdf$Tickets)

ndata<-length(Tixdf$Time)
ndata
ntrain<-ndata-12
train<-(Tixdf[1:ntrain,])
test<-(Tixdf[ntrain+1:12,])

View(train)
View(test)

#Model 1
train.lm.trend1<-lm(Tickets ~ Time, data = train)
summary(train.lm.trend1)

observedtix<-test$Tickets
predictedtix<-predict(train.lm.trend1,test)
predictedtix

#Plots 
plot(train$Time,train$Tickets)
lines(train.lm.trend1$fitted.values, lwd=2)
lines(c(ntrain+1:12),predictedtix,lwd=2, col="blue")

plot(resid(train.lm.trend1)~fitted(train.lm.trend1))
abline(h=0,col="blue")

#RMSE & MAPE 
rmsetrendtix<-rmse(observedtix,predictedtix)
mapetrendtix<-mape(observedtix,predictedtix)*100
mapetrendtix
print(c(rmsetrendtix,mapetrendtix))

#PREDICT FOR FUTURE MONTHS 
Year5 <- data.frame(Time  = c(49,50, 51, 52, 53, 54, 55, 56, 57,58, 59, 60))
View(Year5)
year5predict<-predict(train.lm.trend1,Year5)
year5predict
#------------------------------------------------------------------------------------------

#Model 2 - With Seasonality (part C & D)

train.lm.TS <- lm(Tickets ~ Time+factor(MonthInd),data = train)
summary(train.lm.TS)

observed2<-test$Tickets
observed2
predicted2<-predict(train.lm.TS,test)
predicted2

#Plots 
plot(train$Time,train$Tickets)
lines(train.lm.TS$fitted.values, lwd=2)
lines(c(ntrain+1:12),predicted2,lwd=2, col="blue")
plot(resid(train.lm.TS)~fitted(train.lm.TS))
abline(h=0,col="blue")

#RMSE & MAPE 
rmsetrendTS<-rmse(observed2,predicted2)
mapetrendTS<-mape(observed2,predicted2)*100
print(c(rmsetrendTS,mapetrendTS))

#PREDICT FOR FUTURE MONTHS 
Year5withS <- data.frame(Time  = c(49,50, 51, 52, 53, 54, 55, 56, 57,58, 59, 60), MonthInd=c(1,2,3,4,5,6,7,8,9,10,11,12))
View(Year5withS)
year5predict<-predict(train.lm.TS,Year5withS)
year5predict

###############################################QUESTION2-REVENUE###################################

summary(Rev)
plot(Rev$Year, Rev$Revenue)

#Store in time series 
Revenue.ts<-ts(Rev$Revenue, start = c(1990))
plot(Revenue.ts)

#Train test split 
train.ts <- window(Revenue.ts, start = c(1990), end = c(2008))
train.ts
test.ts<- window(Revenue.ts, start=c(2009))
test.ts

#SimpleModel 
Rev.Simple<-HoltWinters(train.ts, beta = FALSE, gamma = FALSE)
Rev.Simple
plot(Rev.Simple)
lines(Rev.Simple$fitted[,1], lty=2, col="red")

simple.pred<-forecast(Rev.Simple, h=4)
simple.pred

plot(simple.pred, ylab="Revenue", bty="l", xlab="Years")
lines(test.ts)
lines(simple.pred$fitted,col='red')

#RMSE&MAPE
print(c(rmse(test.ts,simple.pred$mean),mape(test.ts,simple.pred$mean)))

#DECOMPOSITION 
#a<-decompose(Revenue.ts,type = c("additive"), filter = NULL)

#Double
Rev.Double<-HoltWinters(train.ts,gamma = FALSE)
Rev.Double
plot(Rev.Double)
lines(Rev.Double$fitted[,1], lty=2, col="red")
double.pred<-forecast(Rev.Double, h=4, level=0)
double.pred
plot(double.pred, ylab="Revenue", bty="l", xlab="Years")
lines(test.ts)
lines(double.pred$fitted,col='red')

#RMSE&MAPE
print(c(rmse(test.ts,double.pred$mean),mape(test.ts,double.pred$mean)))

#FUTURE VALUES
double.pred2<-forecast(Rev.Double, h=8, level=0)
double.pred2

#DOUBLE WITH FIXED PARAMS 
Rev.Double1<-HoltWinters(train.ts,alpha = 0.2, beta =0.15, gamma = FALSE)
plot(Rev.Double1)
lines(Rev.Double1$fitted[,1], lty=2, col="red")
double.pred1<-forecast(Rev.Double1, h=4, level=0)
double.pred1
plot(double.pred1, ylab="Revenue", bty="l", xlab="Years")
lines(test.ts)
lines(double.pred1$fitted,col='red')
print(c(rmse(test.ts,double.pred1$mean),mape(test.ts,double.pred1$mean)))

#Triple Exponential - THROWS AN ERROR 
#Rev.Trip<-HoltWinters(train.ts,seasonal = "multiplicative")
#Rev.Trip
#-----------------------------------------------------------------------

#EXPONENTIAL MODEL - Part(C & D)

#Create Log Data Set 
log <- log(Revenue.ts)
log
plot.ts(log)

train.exp <- window(log, start = c(1990), end = c(2008))
train.exp
test.exp<- window(log, start=c(2009))
test.exp

#SIMPLE EXPONENTIAL 
Rev.Simexp<-HoltWinters(train.exp, beta = FALSE, gamma = FALSE)
Rev.Simexp
plot(Rev.Simexp)
simexp.pred<-forecast(Rev.Simexp, h=4, level=0)
simexp.pred
plot(simexp.pred, ylab="Revenue", bty="l", xlab="time")
lines(test.exp)
lines(simexp.pred$fitted,col='red')
#RMSE&MAPE
print(c(rmse(test.ts,exp(simexp.pred$mean)),mape(test.ts,exp(simexp.pred$mean))))

#Double exponential
Rev.Doubexp<-HoltWinters(train.exp, gamma = FALSE)
Rev.Doubexp
plot(Rev.Doubexp)
Doubexp.pred<-forecast(Rev.Doubexp, h=4, level=0)
Doubexp.pred
plot(Doubexp.pred, ylab="Revenue", bty="l", xlab="time")
lines(test.exp)
lines(Doubexp.pred$fitted,col='red')

#RMSE&MAPE
print(c(rmse(test.ts,exp(Doubexp.pred$mean)),mape(test.ts,exp(Doubexp.pred$mean))))

#Future Values 
Doubexp.pred1<-forecast(Rev.Doubexp, h=8, level=0)
Doubexp.pred1

