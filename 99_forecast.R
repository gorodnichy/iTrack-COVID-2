
# 99_forecast.R
# 
# 
# 
library(GMDH)
data <- ts (dt00$confirmed)

data <- ts (dt00$confirmedSpeed)
out = GMDH::fcast(data)



fcast(data, input = 6, layer = 4, f.number = 1)
fcast(data, input = 6, layer = 2, f.number = 5)

library(forecast)
data %>% ets %>% forecast() %>% autoplot()
data %>% auto.arima() %>% forecast(h=30) %>% autoplot()
data %>% auto.arima() %>% forecast(h=20) %>% autoplot()


