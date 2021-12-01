#IMPORT LIBRARIES

library(readr)
library(x12)
# install.packages("trend")
library(trend)
library(TSA)
library(tseries)
library(dLagM)
library(forecast)
library(dynlm)
setwd("F:/Subbu/RMIT/sem 4/Forecasting/assign 2")
source("MATH1307_utilityFunctions.R")

assign_ds <- read_csv("data1.csv")
head(assign_ds)
str(assign_ds)

#CONVERTING TO TIMESERIES
assign_ds$solar <- ts(assign_ds$solar,start = c(1960,1), end = c(2014,12), frequency = 12)
assign_ds$ppt <- ts(assign_ds$ppt,start = c(1960,1), end = c(2014,12), frequency = 12)
summary(assign_ds$solar)
summary(assign_ds$ppt)

#TIME SERIES PLOT
plot(assign_ds$solar, ylab = "Average Solar Radiations", main = "Time Series Plot of Monthly average solar radiations")
plot(assign_ds$ppt, ylab = "Precipitation", main = "Time Series Plot of Monthly precipitations")

# CORRELATION BETWEEN TWO TIME SERIES
cor_data = ts(assign_ds)
cor(cor_data)

# TEST OF STATIONARITY
adf.test(assign_ds$solar)
PP.test(assign_ds$solar)

#DECOMPOSITION

fit.solarstl <- stl(assign_ds$solar, t.window=15, s.window="periodic", robust=TRUE)
plot(fit.solarstl)

par(mfrow = c(1,2))
acf(assign_ds$solar, main = 'ACF Plot')
pacf(assign_ds$solar, main = 'PACF Plot')
par(mfrow = c(1,1))

# DLMs

model_1 = dlm(x = as.vector(assign_ds$solar), y = as.vector(assign_ds$ppt), q = 12)
summary(model_1)
checkresiduals(model_1)

model_2 = polyDlm(x = as.vector(assign_ds$solar), y = as.vector(assign_ds$ppt), q = 12,
                    k = 2, show.beta = FALSE)
summary(model_2)
checkresiduals(model_2)

model_3 = koyckDlm(x = as.vector(assign_ds$solar), y = as.vector(assign_ds$ppt))
summary(model_3)
checkresiduals(model_3$model)

#ARDLM

for (i in 1:5){
  for(j in 1:5){
    model_4 = ardlDlm(x = as.vector(assign_ds$solar), y = as.vector(assign_ds$ppt),
                       p = i, q = j)
    cat("p=",i,"q=",j, "AIC =", AIC(model_4$model), "BIC = ", BIC(model_4$model), "\n")
  }
}

model4.1.1 = ardlDlm(x = as.vector(assign_ds$solar), y = as.vector(assign_ds$ppt),
                     p = 2, q = 3)
summary(model4.1.1)
checkresiduals(model4.1.1)

MASE(model_1, model_2, model_3, model4.1.1)

########################
  
# DYNAMIC LINEAR MODELS

# TRANSFORMATION

BC = BoxCox.lambda(assign_ds$solar)
BC

BC.Solar = ((assign_ds$solar^(BC)) - 1) / BC
plot(BC.Solar, main = "Time series Plot of BoxCoX transformed series", ylab = "Solar Radiations")

Y.t = BC.Solar

dynlm_1 = dynlm(Y.t ~ L(Y.t , k = 1 ) + trend(Y.t) + season(Y.t))
summary(dynlm_1)

checkresiduals(dynlm_1)

AIC(dynlm_1)



plot(BC.Solar, main = "Time series Plot of BoxCoX transformed series", ylab = "ASX Price", type = "l", col = 'Green')
lines(dynlm_1$fitted.values)

# MASE
accuracy(dynlm_1)
accuracy(dynlm_2)

####################################
# SIMPLE EXPONENTIAL SMOOTHING 
# ses_fit1 <- ses(BC.Solar,initial = 'simple', h=24)
# summary(ses_fit1)
# 
# ses_fit2 <- ses(BC.Solar,initial = 'optimal', h=24)
# summary(ses_fit2)

# HOLT WINTER METHOD

hw_fit1 <- hw(BC.Solar,seasonal="additive", h=24)
summary(hw_fit1)
hw_fit2 <- hw(BC.Solar,seasonal="additive",damped = TRUE, h=24)
summary(hw_fit2)
checkresiduals(hw_fit2)
hw_fit3 <- hw(BC.Solar,seasonal="multiplicative", h=24)
summary(hw_fit3)
hw_fit4 <- hw(BC.Solar,seasonal="multiplicative",exponential = TRUE,h=24)
summary(hw_fit4)

plot(hw_fit2,ylab="Horizontal monthly average solar radiations",
     plot.conf=FALSE, type="l", fcol="RED", xlab="Year")


# ETS - HOLT WINTER METHOD

ETS_HW1 <- ets(BC.Solar,model ="ZZZ",  ic = "aic") 
summary(ETS_HW1)

ETS_HW2 <- ets(BC.Solar,model ="ZZZ", ic = "bic")
summary(ETS_HW2)

# frc.AAdA = forecast(fit.AAdA, h =5)
# AAA_forecast = forecast(ETS_HW1, h = 24)
# plot(AAA_forecast)

###############

# TASK 2

assign_ds2 <- read_csv("data2.csv")
assign_ds2$price <- ts(assign_ds2$price, start = c(2003,3), frequency = 4)
assign_ds2$change <- ts(assign_ds2$change, start = c(2003,3), frequency = 4)
# plot(assign_ds2$price)
plot(ts.intersect(assign_ds2$price, assign_ds2$change),yax.flip = TRUE, main = "Time Series plots of Quarterly change of Melbourne property prices and Population change")

ccf(assign_ds2$price, assign_ds2$change, main = 'CCF Plot')

# dif_ts=ts.intersect(diff(diff(assign_ds2$price,12)),diff(diff(assign_ds2$change,12)))
dif_price = diff(assign_ds2$price, differences = 2)
plot(dif_price, main = 'Second differenced time series plot of property price index')
dif_change = diff(diff(assign_ds2$change,12))           
plot(dif_change, main = 'First differenced time series plot of the population change')
prewhiten(dif_price, dif_change,ylab='CCF', main="Sample CCF after prewhitening of 
          the Quaterly Melbourne PPI and Population change time series")



