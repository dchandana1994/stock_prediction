require("tseries")
require("stats")
require("forecast")
library("timeSeries")

# df1 <- read.csv(file = '/home/rakesh/Rakesh_T/Documents/Code/AIR/Java_hcl_monthly.csv', header = TRUE, sep = ",")
# prediction_period <- 6

# Can not model seasonality with < 2 years data. So leave this thing.
# Cycles can be modeled

hcl_forecasting <- function(df1 , prediction_period)
{

  Trend_Exist <-FALSE

  data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
  data_length <- length(data_ts)

  t1_future <- 1: (data_length+prediction_period)
  t2_future <- t1_future^2
  df_future <- data.frame(t1_future, t2_future)

  kpss_test <- kpss.test(data_ts, null = "T")
  adf_test <- adf.test(data_ts, alternative = "stationary")
  if (adf_test$p.value > 0.05 && kpss_test$p.value < 0.05)
  {
    fit_tslm <- lm( data_ts ~ ., data = df_future[1:data_length,])

    # Moved here on 15th Oct
    fit_sm <- summary(fit_tslm)
    p_value_trend <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])

    if (p_value_trend < 0.05)
    {
      trend_predicted_values <- predict(fit_tslm, newdata = df_future)
      Trend_Exist <- TRUE
    } else
    {
      Trend_Exist <- FALSE
    }

  }

  if (Trend_Exist == TRUE)
  {
    detrend_data <- data_ts - trend_predicted_values[1:data_length]
  } else
  {
    detrend_data <- data_ts
  }

  fit_arima <- auto.arima(detrend_data)
  arima_pred <- predict(fit_arima, n.ahead = prediction_period)
  arima_fitted <- fitted(fit_arima)

  #merged_value <- as.zoo(merge(as.timeSeries(arima_fitted), as.timeSeries(arima_pred$pred)))
  merged_value <- append(arima_fitted, arima_pred$pred)
  merged_value <- ts(merged_value, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)

  trend_adjusted <- 0

  if (Trend_Exist == TRUE)
  {
    trend_adjusted <- trend_predicted_values + merged_value
  }else
  {
    trend_adjusted <- merged_value
  }

 return (trend_adjusted)
}




