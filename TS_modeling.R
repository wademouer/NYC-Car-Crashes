# Time series analysis of nyc car crashes 
#Data/Package Loading ----
# NOTE: Will load aggregated version, so raw data not necessary
# library(readr)
# crashes <- read_csv("Motor_Vehicle_Collisions_-_Crashes.csv", 
#                     col_types = cols(`CRASH DATE` = col_date(format = "%m/%d/%Y"), 
#                                      LOCATION = col_skip(), `CONTRIBUTING FACTOR VEHICLE 4` = col_skip(), 
#                                      `CONTRIBUTING FACTOR VEHICLE 5` = col_skip(), 
#                                      `VEHICLE TYPE CODE 4` = col_skip(), 
#                                      `VEHICLE TYPE CODE 5` = col_skip()))
# View(crashes)


# install.packages('tseries')
# install.packages('tsbox')
# install.packages('forecast',dependencies = T)
# install.packages(c('expsmooth','lmtest','zoo','seasonal','haven','fma'))
library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tsbox)

library(tidyverse)
library(lubridate)
library(summarytools)

theme_set(theme_minimal())
# Manipulation - weekly/monthly

#monthly_count <- weekly_count %>% group_by(year, month) %>% summarise(crashes = sum(n))
#rm(crashes)

load('monthly_count.RData')

#trimming off 2019 and 2020 - 2019 will be test data. Not touching 2020

series <- ts(monthly_count$crashes, start = c(2012, 7), end = c(2018 , 12) , frequency = 12)
actual_2019 = monthly_count %>% filter(year == 2019) %>% select(crashes) %>% pull()
autoplot(series)
ggsubseriesplot(series)


stl_decomp = stl(series, s.window = 12)
autoplot(stl_decomp)
#clear seasonality, trend is interesting


#Functions to use later ----
plot_fit <- function(model, with_actual = F){
  actual = data.frame(ind = index(model$x), values = as.matrix(model$x)[,1], model = 'actual')
  fit = data.frame(ind = index(model$fitted), values = as.matrix(model$fitted)[,1], model = 'fit')
  fcast = data.frame(ind = index(model$mean), values = as.matrix(model$mean)[,1], model = 'forecast')
  
  valid <- data.frame(ind = index(model$mean), values = actual_2019, model = 'actual')
  
  df = rbind(actual, fit, fcast)
  
  gg <- df %>% ggplot(aes(x = ind, y = values, color = model, linetype = model)) +
    geom_line() +
    scale_color_manual(values = c('black', 'blue', 'red'))+
    scale_linetype_manual(values = c(1,2,1)) + 
    theme_minimal()+
    labs(title = 'Forecast compared to fit', 
         subtitle = model$method,
         y = 'Monthly Crashes',
         x = 'Time', 
         legend = 'Model')
  if(with_actual)
    gg <- gg + geom_line(data = valid, aes(x = ind, y = values), color = 'black', linetype = 1)
  
  return(gg)
}

#training mae and mape
mae <- function(pred, actual){
  return(mean(abs(actual - pred)))
}
mape <- function(pred, actual){
  return(mean(abs((actual - pred)/actual)))
}



#Exponential smoothing----


hw_cars_damped <- hw(series, seasonal = 'additive', damped = T, h = 12)
autoplot(hw_cars_damped)

hw_mult <- hw(series, seasonal = 'multiplicative', damped = T, h = 12)
autoplot(hw_mult)


plot_fit(hw_cars_damped)
plot_fit(hw_mult)


#HW Damped additive
mae(hw_cars_damped$fitted, hw_cars_damped$x)
mape(hw_cars_damped$fitted, hw_cars_damped$x)

#HW mult
mae(hw_mult$fitted, hw_mult$x)
mape(hw_mult$fitted, hw_mult$x)

#ARIMA ----
autoplot(series)
ggtsdisplay(series, plot.type = 'histogram', smooth = T, lag.max = 10, main = 'Non-differenced Car Crashes')


ndiffs(series, test = 'adf')
#1 difference

series.diff <- series %>% diff(differences = 1)

ndiffs(series, test = 'adf')
ggtsdisplay(series.diff, plot.type = 'partial', smooth = T, lag.max = 10, main = "First-differenced Car Crashes")


#model
Arima.fit <- auto.arima(series, seasonal = T, d = 1)

Arima.fit %>% forecast(h = 12) %>% plot_fit()


LB = NULL
for(i in 1:10){
  LB[i] <- Box.test(Arima.fit$residuals, lag = i, type = 'Ljung')$p.value
}
data.frame('p.value' = LB, 'lags' = seq(1,10)) %>% ggplot(aes(x = lags, y = p.value)) +
  geom_col() +
  geom_hline(yintercept = .05, linetype = 'dashed') +
  scale_x_continuous("Number of Lags", breaks = seq(2,10,2), labels = seq(2,10,2)) +
  labs(y = 'P Value', title = 'Ljung-Box test for model residuals')


mae(Arima.fit$fitted, Arima.fit$x)
mape(Arima.fit$fitted, Arima.fit$x)



#Forecasting and checking validation ----

checkresiduals(Arima.fit)
checkresiduals(hw_cars_damped)
checkresiduals(hw_mult)

hw_damped <- forecast(hw_cars_damped, h = 12)
hw_mult_fc <- forecast(hw_mult, h = 12)
arima.forecast <- forecast(Arima.fit, h = 12)

actual_2019 = monthly_count %>% filter(year == 2019) %>% select(crashes) %>% pull()

actual_2019

mae(hw_damped$mean, actual_2019)
mape(hw_damped$mean, actual_2019)

mae(hw_mult_fc$mean, actual_2019)
mape(hw_mult_fc$mean, actual_2019)

mae(arima.forecast$mean, actual_2019)
mape(arima.forecast$mean, actual_2019)

plot_fit(hw_mult, with_actual = T)

#ggsave(width = 8, height = 4, filename = 'images/bestfit.png')

plot_fit(hw_cars_damped, with_actual = T)

plot_fit(forecast(Arima.fit, h = 12), with_actual = T)
