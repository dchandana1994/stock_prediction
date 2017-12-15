require("tseries")
require("stats")
require("forecast")
library("timeSeries")


which_trend <- function(t1_future,  data_ts, data_length)
{
  t2_future <- t1_future^2
  t3_future <- t1_future^3

  df_future2 <- data.frame(t1_future, t2_future)
  df_future3 <- data.frame(t1_future, t2_future,t3_future)

  fit_tslm1 <- lm( data_ts ~ t1_future[1:data_length])
  fit_sm <- summary(fit_tslm1)
  p_value_trend1 <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])

  fit_tslm2 <- lm( data_ts ~ ., data = df_future2[1:data_length,])
  fit_sm <- summary(fit_tslm2)
  p_value_trend2 <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])

  fit_tslm3 <- lm( data_ts ~ ., data = df_future3[1:data_length,])
  fit_sm <- summary(fit_tslm3)
  p_value_trend3 <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])

  overall_fit <- NULL
  if(p_value_trend1 <0.05)
  {
    overall_fit <-fit_tslm1
  }else
  {
    if(p_value_trend2 <0.05)
    {
      overall_fit <-fit_tslm2
    }else
    {
      if(p_value_trend3 <0.05)
      {
        overall_fit <-fit_tslm3
      }
    }
  }

  return(overall_fit)
}


# df1 <- read.csv(file = '/home/rakesh/Rakesh_T/Documents/Code/AIR/Java_apps_monthly.csv', header = TRUE, sep = ",")
# prediction_period <- 6

# Can not model seasonality with < 2 years data. So leave this thing.
# Cycles can be modeled

hcl_forecasting2 <- function(df1 , prediction_period)
{

  Trend_Exist <-FALSE

  Method_l <- 0

  data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
  data_length <- length(data_ts)
  used_samples <- 3

  t1_future <- 1: (data_length+prediction_period)
  t2_future <- t1_future^2
  df_future <- data.frame(t1_future, t2_future)

  kpss_test <- kpss.test(data_ts, null = "T")
  adf_test <- adf.test(data_ts, alternative = "stationary")
  if (adf_test$p.value > 0.05 && kpss_test$p.value < 0.05)
  {
    #Test then use
    fit_tslm <- which_trend(t1_future,  data_ts, data_length)
    #fit_tslm <- lm( data_ts ~ ., data = df_future[1:data_length,])

    if (!is.null(fit_tslm))
    {
      fit_sm <- summary(fit_tslm)
      p_value_trend <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])

      if (p_value_trend < 0.05)
      {
        trend_predicted_values <- predict(fit_tslm, newdata = df_future)
        Trend_Exist <- TRUE
      } else
      {
        Trend_Exist <- FALSE
        Method_l <- 2
      }
    }

    #print(Trend_Exist)
  }

  #Trend_Exist <- FALSE
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

  # Calculate 3 step linear regression and compare information content
  # based on AIC for ARIMA fitten model and simple linear regression
  x1 <- detrend_data[1:(data_length - used_samples)]
  x2 <- detrend_data[2:(data_length - used_samples+1)]
  x3 <- detrend_data[3:(data_length - used_samples+2)]
  train_y <- detrend_data[4:(data_length - used_samples+3)]
  trainingdata <- cbind(x1,x2,x3, train_y)
  colnames(trainingdata) <- c("x1","x2","x3","train_y")
  fit_lm <- lm(train_y~x1+x2+x3, data.frame(trainingdata))

  trend_adjusted <- 0
  ar_sm <- summary(fit_lm)
  ar_p <- 1 - pf(ar_sm$fstatistic[1], ar_sm$fstatistic[2], ar_sm$fstatistic[3])
  arima_number_of_terms <- sum(fit_arima$arma) - fit_arima$arma[5]

  if ( (AIC(fit_lm) >= AIC(fit_arima) | ar_p > 0.075) & (arima_number_of_terms != 0))
  {


    if (Trend_Exist == TRUE)
    {
      Method_l <- 3
      trend_adjusted <- trend_predicted_values + merged_value
    }else
    {
      Method_l <- 4
      trend_adjusted <- merged_value
    }

    data_MAPE <-  (trend_adjusted[1:(data_length-used_samples)] -
                     data_ts[(used_samples+1):data_length])/data_ts[(used_samples+1):data_length]

  }else
  {


    #Predict in iterations here

    lm_pred <- matrix(,nrow = prediction_period, ncol = 1)
    x_test <- detrend_data
    trainingdata <- cbind(x1,x2,x3, train_y)
    for (n in 1:prediction_period)
    {
      temp1 <- x_test[(length(x_test)-2)]
      temp2 <- x_test[(length(x_test)-1)]
      temp3 <- x_test[length(x_test)]
      trainingdata = rbind(trainingdata, c(temp1, temp2 , temp3, 0))

      temp4 <- predict(fit_lm, data.frame(trainingdata))
      #print(temp4)
      lm_pred[n] <- temp4[length(temp4)]
      x_test = append(x_test,lm_pred[n])

      #break
    }


    if (Trend_Exist == TRUE)
    {
      Method_l <- 5
      trend_adjusted <- trend_predicted_values[(used_samples+1):(data_length + prediction_period)] + temp4
    }else
    {
      Method_l <- 6
      trend_adjusted <- temp4
    }

    data_MAPE <-  (trend_adjusted[1:(data_length-used_samples)] -
                     data_ts[(used_samples+1):data_length])/data_ts[(used_samples+1):data_length]

  }

  data_MAPE <- mean(abs(data_MAPE))
#
#   Trend_Exist <-FALSE
#
#   Method_l <- 0
#
#   data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
#   data_length <- length(data_ts)
#   used_samples <- 3
#
#   t1_future <- 1: (data_length+prediction_period)
#   t2_future <- t1_future^2
#   df_future <- data.frame(t1_future, t2_future)
#
#   kpss_test <- kpss.test(data_ts, null = "T")
#   adf_test <- adf.test(data_ts, alternative = "stationary")
#   if (adf_test$p.value > 0.05 && kpss_test$p.value < 0.05)
#   {
#     #fit_tslm <- lm( data_ts ~ ., data = df_future[1:data_length,])
#     fit_tslm <- which_trend(t1_future, data_length)
#
#     fit_sm <- summary(fit_tslm)
#     p_value_trend <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])
#
#     Method_l <- 1
#     if (!is.null(fit_tslm))
#     {
#       if (p_value_trend < 0.05)
#       {
#         trend_predicted_values <- predict(fit_tslm, newdata = df_future)
#         Trend_Exist <- TRUE
#       } else
#       {
#         Trend_Exist <- FALSE
#         Method_l <- 2
#       }
#     }
#
#   }
#
#   #Trend_Exist <- FALSE
#   if (Trend_Exist == TRUE)
#   {
#     detrend_data <- data_ts - trend_predicted_values[1:data_length]
#   } else
#   {
#     detrend_data <- data_ts
#   }
#
#   fit_arima <- auto.arima(detrend_data)
#   arima_pred <- predict(fit_arima, n.ahead = prediction_period)
#   arima_fitted <- fitted(fit_arima)
#
#   #merged_value <- as.zoo(merge(as.timeSeries(arima_fitted), as.timeSeries(arima_pred$pred)))
#   merged_value <- append(arima_fitted, arima_pred$pred)
#   merged_value <- ts(merged_value, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
#
#   # Calculate 3 step linear regression and compare information content
#   # based on AIC for ARIMA fitten model and simple linear regression
#   x1 <- detrend_data[1:(data_length - used_samples)]
#   x2 <- detrend_data[2:(data_length - used_samples+1)]
#   x3 <- detrend_data[3:(data_length - used_samples+2)]
#   train_y <- detrend_data[4:(data_length - used_samples+3)]
#   trainingdata <- cbind(x1,x2,x3, train_y)
#   colnames(trainingdata) <- c("x1","x2","x3","train_y")
#   fit_lm <- lm(train_y~x1+x2+x3, data.frame(trainingdata))
#
#   trend_adjusted <- 0
#   ar_sm <- summary(fit_lm)
#   ar_p <- 1 - pf(ar_sm$fstatistic[1], ar_sm$fstatistic[2], ar_sm$fstatistic[3])
#   arima_number_of_terms <- sum(fit_arima$arma) - fit_arima$arma[5]
#
#   if ( (AIC(fit_lm) >= AIC(fit_arima) | ar_p > 0.075) & (arima_number_of_terms != 0))
#   {
#
#     if (Trend_Exist == TRUE)
#     {
#       Method_l <- 3
#       trend_adjusted <- trend_predicted_values + merged_value
#     }else
#     {
#       Method_l <- 4
#       trend_adjusted <- merged_value
#     }
#     data_MAPE <-  (trend_adjusted[1:(data_length-used_samples)] -
#                      data_ts[(used_samples+1):data_length])/data_ts[(used_samples+1):data_length]
#
#   }else
#   {
#
#     #Predict in iterations here
#
#     lm_pred <- matrix(,nrow = prediction_period, ncol = 1)
#     x_test <- detrend_data
#     trainingdata <- cbind(x1,x2,x3, train_y)
#     for (n in 1:prediction_period)
#     {
#       temp1 <- x_test[(length(x_test)-2)]
#       temp2 <- x_test[(length(x_test)-1)]
#       temp3 <- x_test[length(x_test)]
#       trainingdata = rbind(trainingdata, c(temp1, temp2 , temp3, 0))
#
#       temp4 <- predict(fit_lm, data.frame(trainingdata))
#       #print(temp4)
#       lm_pred[n] <- temp4[length(temp4)]
#       x_test = append(x_test,lm_pred[n])
#
#       #break
#     }
#
#
#     if (Trend_Exist == TRUE)
#     {
#       Method_l <- 5
#       trend_adjusted <- trend_predicted_values[(used_samples+1):(data_length + prediction_period)] + temp4
#     }else
#     {
#       Method_l <- 6
#       trend_adjusted <- temp4
#     }
#
#     data_MAPE <-  (trend_adjusted[1:(data_length-used_samples)] -
#                      data_ts[(used_samples+1):data_length])/data_ts[(used_samples+1):data_length]
#
#   }
#
#   data_MAPE <- mean(abs(data_MAPE))

  return (trend_adjusted)
}

## Only to send method name######

hcl_forecasting_method2 <- function(df1 , prediction_period)
{

  Trend_Exist <-FALSE

  Method_l <- 0

  data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
  data_length <- length(data_ts)
  used_samples <- 3

  t1_future <- 1: (data_length+prediction_period)
  t2_future <- t1_future^2
  df_future <- data.frame(t1_future, t2_future)

  kpss_test <- kpss.test(data_ts, null = "T")
  adf_test <- adf.test(data_ts, alternative = "stationary")
  if (adf_test$p.value > 0.05 && kpss_test$p.value < 0.05)
  {
    #fit_tslm <- lm( data_ts ~ ., data = df_future[1:data_length,])
    fit_tslm <- which_trend(t1_future, data_length)

    fit_sm <- summary(fit_tslm)
    p_value_trend <- 1 - pf(fit_sm$fstatistic[1], fit_sm$fstatistic[2], fit_sm$fstatistic[3])

    Method_l <- 1
    if (!is.null(fit_tslm))
    {
      if (p_value_trend < 0.05)
      {
        trend_predicted_values <- predict(fit_tslm, newdata = df_future)
        Trend_Exist <- TRUE
      } else
      {
        Trend_Exist <- FALSE
        Method_l <- 2
      }
    }

  }

  #Trend_Exist <- FALSE
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

  # Calculate 3 step linear regression and compare information content
  # based on AIC for ARIMA fitten model and simple linear regression
  x1 <- detrend_data[1:(data_length - used_samples)]
  x2 <- detrend_data[2:(data_length - used_samples+1)]
  x3 <- detrend_data[3:(data_length - used_samples+2)]
  train_y <- detrend_data[4:(data_length - used_samples+3)]
  trainingdata <- cbind(x1,x2,x3, train_y)
  colnames(trainingdata) <- c("x1","x2","x3","train_y")
  fit_lm <- lm(train_y~x1+x2+x3, data.frame(trainingdata))

  trend_adjusted <- 0
  ar_sm <- summary(fit_lm)
  ar_p <- 1 - pf(ar_sm$fstatistic[1], ar_sm$fstatistic[2], ar_sm$fstatistic[3])
  arima_number_of_terms <- sum(fit_arima$arma) - fit_arima$arma[5]

  # Here use Arima or 3 lags AR or Seasonality
  if ( (AIC(fit_lm) >= AIC(fit_arima) | ar_p > 0.075) & (arima_number_of_terms != 0))
  {

    if (Trend_Exist == TRUE)
    {
      Method_l <- 3
      trend_adjusted <- trend_predicted_values + merged_value
    }else
    {
      Method_l <- 4
      trend_adjusted <- merged_value
    }
    data_MAPE <-  (trend_adjusted[1:(data_length-used_samples)] -
                     data_ts[(used_samples+1):data_length])/data_ts[(used_samples+1):data_length]

  }else
  {

    #Predict in iterations here

    lm_pred <- matrix(,nrow = prediction_period, ncol = 1)
    x_test <- detrend_data
    trainingdata <- cbind(x1,x2,x3, train_y)
    for (n in 1:prediction_period)
    {
      temp1 <- x_test[(length(x_test)-2)]
      temp2 <- x_test[(length(x_test)-1)]
      temp3 <- x_test[length(x_test)]
      trainingdata = rbind(trainingdata, c(temp1, temp2 , temp3, 0))

      temp4 <- predict(fit_lm, data.frame(trainingdata))
      #print(temp4)
      lm_pred[n] <- temp4[length(temp4)]
      x_test = append(x_test,lm_pred[n])

      #break
    }


    if (Trend_Exist == TRUE)
    {
      Method_l <- 5
      trend_adjusted <- trend_predicted_values[(used_samples+1):(data_length + prediction_period)] + temp4
    }else
    {
      Method_l <- 6
      trend_adjusted <- temp4
    }

    data_MAPE <-  (trend_adjusted[1:(data_length-used_samples)] -
                     data_ts[(used_samples+1):data_length])/data_ts[(used_samples+1):data_length]

  }

  data_MAPE <- mean(abs(data_MAPE))

  return (Method_l)

}
