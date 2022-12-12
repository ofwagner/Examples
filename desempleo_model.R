library(readr)
library(magrittr)
library(forecast)





desempleo_data <- read_delim("desempleo.csv", 
                        ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                        locale = locale(decimal_mark = ".", grouping_mark = ","), 
                        trim_ws = TRUE, skip = 0)


desempleo_serie <- ts(desempleo_data$desempleo, frequency=12)

desempleo_decomp <- decompose(desempleo_serie)

plot(desempleo_decomp)

desempleo_noise <- desempleo_decomp$random

plot(desempleo_noise)

Acf(desempleo_noise)
Pacf(desempleo_noise)



best_arima <- auto.arima(desempleo_serie)


summary(best_arima)


Acf(best_arima$residuals)
Pacf(best_arima$residuals)

forecast <- forecast(best_arima, h = 20, level = c(80, 90, 95, 99))


plot(forecast)
plot(forecast, 20)



accuracy(f=forecast, x=desempleo_serie[140:156] )



prba <- auto.arima(desempleo_noise)


summary(prba)
