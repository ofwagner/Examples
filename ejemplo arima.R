library(readr)
library(magrittr)
library(forecast)





INPC_data <- read_delim("INPC_data.csv", 
                        ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), 
                        trim_ws = TRUE, skip = 2)


INPC_serie <- ts(INPC_data$INPC, frequency=12)

INPC_decomp <- decompose(INPC_serie)

plot(INPC_decomp)

INPC_noise <- INPC_decomp$random

plot(INPC_noise)

Acf(INPC_noise)
Pacf(INPC_noise)

INPC_noise

best_arima <- auto.arima(INPC_serie)


summary(best_arima)


Acf(best_arima$residuals)
Pacf(best_arima$residuals)

forecast <- forecast(best_arima, h = 50)

plot(forecast)