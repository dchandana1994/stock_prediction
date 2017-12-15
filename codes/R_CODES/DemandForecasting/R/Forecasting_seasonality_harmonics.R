require("tseries")
require("stats")
require("forecast")
library("timeSeries")

# df1 <- read.csv(file = '/home/rakesh/Rakesh_T/Documents/Code/AIR/Java_hcl_monthly.csv', header = TRUE, sep = ",")
# prediction_period <- 6

# Can not model seasonality with < 2 years data. So leave this thing.
# Cycles can be modeled

hcl_decomposition <- function(df1)
{

  Trend_Exist <-FALSE

  data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
  data_length <- length(data_ts)
  fit_decomp <- decompose(data_ts)
  fit_trend <- fit_decomp$trend

  return (fit_decomp)
}

hcl_decomposition_stl <- function(df1)
{

  Trend_Exist <-FALSE

  data_ts <- ts(df1$Frequency, start = c(df1[1,]$Year,df1[1,]$month), frequency = 12)
  data_length <- length(data_ts)
  fit_stl <- stl(data_ts,s.window="periodic",robust=TRUE)
  return (fit_stl)
}



