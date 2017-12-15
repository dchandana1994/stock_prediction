
hcl_forecasting_stl <- function(df1 , prediction_period)
{
  data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
  data_length <- length(data_ts)

  length_data <- length(data_ts)
  tt <- 1:length_data
  if(frequency(data_ts)>1 & length_data> 24){
    fit_stl <- stl(data_ts,s.window="periodic",robust=TRUE)
    resid <- fit_stl$time.series[,3]
  }else{
    resid <- residuals(loess(data_ts ~ tt))
  }

  forecast_value <- forecast(fit_stl, h = prediction_period, method = 'arima')

#  return (data.frame(forecast_value))
  return (forecast_value)
}
