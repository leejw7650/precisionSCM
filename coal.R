library(gtools)
library(rstudioapi)
library(aTSA)
library(tseries)
library(dplyr)
library(itsmr)

setwd(dirname(getActiveDocumentContext()$path))

rm(list = ls())

data_week_raw <- read.csv("data_oil_202205_weekly.csv", header = TRUE)
data_month_raw <- read.csv("data_oil_202205_monthly.csv", header = TRUE)

head(data_week_raw)

ts.plot(data_week_raw[,"WTI"])

mon <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
mon2 <- c('01','02','03','04','05','06','07','08','09','10','11','12')
for(i in 1:nrow(data_week_raw)){
  date <- data_week_raw[i,1]
  month <- mon[as.numeric(substr(date,6,7))]
  year <- substr(date,3,4)
  data_week_raw$date2[i] <- paste0(month,'.',year)
}

colnames(data_month_raw)
data_month_IP <- data_month_raw[,c("Date","US_IP","China_IP","EU19_IP","US.MB","EU.MB")]
colnames(data_month_IP) <- c("date2","US_IP","China_IP","EU19_IP","US.MB","EU.MB")

data_week_raw_mod <- data_week_raw[,-c(14:18)]
data <- left_join(data_week_raw_mod, data_month_IP, by = "date2")
data <- data[c(1:700),]

write.csv(data, file = "data_merged.csv")

for(i in data_month_raw$Date[-c(1:12)]){
  index <- which(data$date2 == i)
  data[head(index)[2]:tail(index,1),][,c("US_IP","China_IP","EU19_IP","US.MB","EU.MB")] <-  NA
}

data_linear <- data

data_linear[,"US_IP"][1:692] <- approx(data_linear[,"US_IP"][1:692],n = 692)$y
data_linear[,"China_IP"][1:688] <- approx(data_linear[,"China_IP"][1:688],n = 688)$y
data_linear[,"EU19_IP"][1:688] <- approx(data_linear[,"EU19_IP"][1:688],n = 688)$y
data_linear[,"US.MB"][1:692] <- approx(data_linear[,"US.MB"][1:692],n = 692)$y
data_linear[,"EU.MB"][1:692] <- approx(data_linear[,"EU.MB"][1:692],n = 692)$y

write.csv(data_linear, file = "data_linear_interpolate.csv")

################################################

head(data)
par(mfrow=c(1,1))
for(i in 1:nrow(data)){
  date <- data[i,14]
  month <- mon2[which(mon == substr(date,1,3))]
  year <- substr(date,5,6)
  data$date2[i] <- paste0(year,".",month)
}

ts.plot(data$WTI, ylab = "Price", main = "Oil price(monthly)")

WTI <- data %>% group_by(date2) %>% summarise(mean(WTI)) ; WTI <- WTI$'mean(WTI)'
WTI <- ts(WTI, start = 2009, frequency = 12)
ts.plot(WTI, ylab = "Price", main = "Oil price(weekly)")


par(mfrow = c(2,1))
acf(WTI, lag = 100)
pacf(WTI, lag = 100)

par(mfrow = c(1,1))
WTI_diff <- diff(WTI) ; n <- length(WTI_diff)
ts.plot(WTI_diff, main = "diff(WTI)", ylab = "price")
par(mfrow = c(2,1))
acf(WTI_diff, lag = 100)
pacf(WTI_diff, lag = 100)

qqnormPlot(WTI_diff)

test(WTI_diff) ; jarqueberaTest(WTI_diff)

n <- length(WTI_diff)
n_train <- floor(n * 0.85)
n_test <- n - n_train
train <- WTI_diff[1:n_train]
test <- WTI_diff[-c(1:n_train)]

par(mfrow=c(2,1))
acf(train,lag = 100)
pacf(train,lag = 100)

fit <- arima(train, order = c(1,0,1))

test(fit$residuals)

# n_train <- floor(n * 0.7)
# n_test <- n - n_train
# 
# train <- ts(WTI_diff[1:n_train], start = 1)
# test <- ts(WTI_diff[(n_train+1):n], start = n_train+1)
# par(mfrow = c(1,1))
# ts.plot(train, col = 'red', xlim = c(0,150)) ; lines(test, col = 'blue')
# 
# test(train) ; jarqueberaTest(train) ; adf.test(train, k = 5)

# comb <- expand.grid(rep(list(0:5), 2))
# mspe <- c()
#
# for(i in 1:nrow(comb)){
#   fit <- arima(train, order = c(comb[i,1],0,comb[i,2]))
#   prediction <- forecast(fit,45) ; title(paste0("arima(",comb[i,1],",",0,",",comb[i,2],")"))
#   lines(test,col = 'blue')
#   mspe <- c(mspe, mean((prediction[,2] - test[1:45])^2))
# }

order_selecting_arima <- function(train_set, valid_set, step = 10, d = 0){
  n_train <- length(train)
  train <- ts(train_set, start = 1)
  valid <- ts(valid_set, start = length(train_set) + 1)
  
  comb <- expand.grid(rep(list(0:5), 2))
  mspe <- c()
  for(i in 1:nrow(comb)){
    fit <- arima(train, order = c(comb[i,1],d,comb[i,2]), method = 'ML')
    prediction <- aTSA::forecast(fit, step) ; title(paste0("arima(",comb[i,1],",",d,",",comb[i,2],")"))
    mspe <- c(mspe, mean((prediction[,2] - valid[1:step])^2))
    lines(ts(valid[1:step],start = n_train + 1),col = 'blue')
  }
  
  mspe <- data.frame(mspe, row.names = paste0("(",comb[,1],",",d,",",comb[,2],")"))
  
  structure(list(best.order = comb[(which(mspe == min(mspe))),], MSPE = mspe))
}

# select order by Information Criterion(especially AIC)
comb <- expand.grid(rep(list(0:5), 2))

aic <- c()
for(i in 1:nrow(comb)){
  fit <- arima(train, order = c(comb[i,1],0,comb[i,2]), method = "ML")
  aic <- c(aic, fit$aic)
}
comb[which(aic == min(aic)),]
fit <- arima(train, order = c(0,0,1))
test(fit$residuals)



# select order by prediction error

par(mfrow = c(1,1))

validation <- train[(n_train - n_test + 1):n_train]
train <- train[1:(n_train - n_test)]

result <- order_selecting_arima(train, validation, step = 10)
result$best.order
result$MSPE

arima(WTI, order = c(1,1,3))
prediction <- aTSA::forecast(arima(WTI_diff, order = c(3,0,4)), 10)

ts.plot(WTI_diff)

fit <- arima(train, order = c(3,0,4))

test(fit$residuals)

# predict test set

train
validation
test



fit1 <- arima(c(train, validation), order = c(0,0,1), method = "ML")
fit2 <- arima(c(train, validation), order = c(3,0,4), method = "ML")

prediction1 <- aTSA::forecast(fit1, n_test)
title("ARIMA(0,0,1)") ; lines(lines(ts(test,start = n_train + 1),col = 'blue'))
prediction2 <- aTSA::forecast(fit2, n_test)
title("ARIMA(3,0,4)") ; lines(lines(ts(test,start = n_train + 1),col = 'blue'))

MSPE1 <- mean((prediction1[,2] - test)^2)
MSPE2 <- mean((prediction2[,2] - test)^2)




