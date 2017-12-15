
'''
This Code Predicts the Stock Market data. 
'''

import urllib3
import json
import pytz
import fnmatch
import os
import pandas as pd
import pandas as pd
import datetime
from dateutil.relativedelta import relativedelta
import numpy as np
import math
import datetime
import statsmodels.api as sm
from rpy2.robjects.packages import importr
from rpy2.rinterface import RRuntimeError
from rpy2.rinterface import NARealType
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
import scipy as sp
from sklearn import linear_model
from yahoo_finance import Share
import pandas_datareader as web
from pandas.tseries.offsets import BDay
from matplotlib.dates import DateFormatter, WeekdayLocator,    DayLocator, MONDAY
from matplotlib.finance import candlestick_ohlc
import pickle
import os
from  codes.Companies import constituent_data,ApiDetails,Company_data,stock_time_zone
import logging
from codes.Commons import Constants


# class Company_data():
#     def __init__(self, company_name, company_symbol, start_time, end_time, raw_json_path = None ):
#         self.raw_data_path       = raw_json_path
#         self.company_name        = company_name
#         self.company_symbol      = company_symbol
#         self.start_time          = start_time
#         self.end_time            = end_time
#
#
#
#     def set_pandas_data_file(self, format = "csv"):
#         '''
#
#         :param company_name:
#         :param company_symbol:
#         :param start_date:
#         :param end_date:
#         :param format:
#         :return:
#         '''
#         with open(self.raw_data_path, 'r') as fp:
#             data = json.load(fp)
#
#
#         df_data         = pd.DataFrame(data['_Source']['Time Series (Daily)']).transpose()
#         self.meta_data  = data['_Source']['Meta Data']
#
#         df_data.columns = ['Open','High','Low','Close','Volume']
#         for col in df_data.columns:
#             df_data[col]  = df_data[col].astype('float64')
#
#
#         df_data.index   = df_data.index.to_datetime()
#         df_data['Time_Stamp'] = df_data.index
#
#
#         self.company_data = df_data[(df_data['Time_Stamp']>= self.start_time)  & (df_data['Time_Stamp'] <= self.end_time)]
#         self.actual_start_time = min(self.company_data['Time_Stamp'])
#         self.actual_end_teime  = max(self.company_data['Time_Stamp'])
#         del self.company_data['Time_Stamp']
#
#
#
#     def get_company_data(self):
#         return self.company_data
#
#
#
#
#     def get_pandas_data_mongo(self, start_time, end_time, format = "csv"):
#         '''
#
#         :param company_name:
#         :param company_symbol:
#         :param start_date:
#         :param end_date:
#         :param format:
#         :return:
#         '''
#
#
#
# class stock_time_zone():
#     def __init__(self,time_zone = 'US/Eastern'):
#         self.time_zone = time_zone
#
#
#     def get_timezone_time(self,dt_time = datetime.datetime.today()):
#         set_timezone  = pytz.timezone(self.time_zone)
#         time_timezone = set_timezone.localize(dt_time)
#         return time_timezone
#
#






def run_model(company_obj,Forecasting_horizon):
    regress = linear_model.LinearRegression(fit_intercept=True)
    failed_combination = {}
    list_columns = ["Close", "Open", "High", "Low"]

    Company_Name = company_obj.company_name
    Company_Code = company_obj.company_symbol
    stock_data   = company_obj.company_data
    start        = company_obj.actual_start_time
    end          = company_obj.actual_end_teime





    final_output = {Company_Name: {}}

    ##########################################################################

    print("Buliding Model for :", Company_Name)
    for typeof in list_columns:
        print("typeof:", typeof)

        final_output[Company_Name][typeof] = {}
        comb_df = stock_data[[typeof]]
        comb_df['Time_Line'] = comb_df.index
        comb_df = comb_df.rename(columns={typeof: 'Frequency'})
        comb_df = comb_df.reset_index(drop=True)
        comb_df['index'] = comb_df.index
        comb_df['Year'] = comb_df.Time_Line.dt.year
        comb_df['month'] = comb_df.Time_Line.dt.month



        ###############################################################





        # This is were the pandas dataframe is converted into 'r' object.
        r_skill_print_freq = pandas2ri.py2ri(comb_df)
        # Forecasting_horizon let you forecast from the end_date  till end_date + Forecasting_horizon.


        r_prediction_period = ro.IntVector([Forecasting_horizon])

        # In[112]:


        r_forecasted_values_stl = r_forecast_obj.hcl_forecasting_stl(r_skill_print_freq, r_prediction_period)
        forecasted_values_stl = r_forecasted_values_stl.rx()
        r_forecasted_values = list(forecasted_values_stl[8])
        for n_temp in range(0, Forecasting_horizon):
            r_forecasted_values.append(forecasted_values_stl[3][n_temp])
        for temp_n in range(0, len(r_forecasted_values)):
            if r_forecasted_values[temp_n] < 0:
                r_forecasted_values[temp_n] = 0






        # Total number of actual Data points
        length_actual = comb_df.shape[0]
        # Total Length of Forecasted values.
        length_forecast = len(r_forecasted_values)
        # Total Length of predictions.
        no_of_predictions = length_forecast

        rng = pd.date_range(start, periods=no_of_predictions)
        ts = rng.to_period()
        rng = ts.to_timestamp()

        actual_time_series = pd.Series(np.array(comb_df.Frequency), index=comb_df.Time_Line)
        pred_time_series = pd.Series(np.array(r_forecasted_values), index=rng)

        error = comb_df.Frequency[3:] - r_forecasted_values[3:length_actual]

        MAPE = np.abs(error / comb_df.Frequency[3:])
        MAPE = np.mean(MAPE)
        Uncertainity_measurement = []
        Error_variance = np.var(error)
        for i_temp in range(0, Forecasting_horizon):
            Uncertainity_measurement.append(np.sqrt((i_temp + 1) * Error_variance))

        actual_df = actual_time_series.to_frame()
        actual_df.columns = ["Actual"]
        time_indices = actual_df.index

        forecast_time_list = []
        for i in range(1, Forecasting_horizon + 1):
            forecast_time_list += [end + BDay(i)]

        forecast_DatetimeIndex = pd.DatetimeIndex(forecast_time_list)

        final_forecast_DTIndex = time_indices.append(forecast_DatetimeIndex)

        forecasted_df = pd.DataFrame(data={"Forecasted": r_forecasted_values}, index=final_forecast_DTIndex)

        final_combine_data = pd.concat([actual_df, forecasted_df], axis=1)






























        ###########################################################################


        final_output[Company_Name][typeof]["Forecast"] = final_combine_data.to_dict()
        final_output[Company_Name][typeof]["MAPE"] = MAPE
        final_output[Company_Name][typeof]["uncertainity"] = Uncertainity_measurement
        final_output[Company_Name]["Forecast_Horizon"] = Forecasting_horizon
        final_output[Company_Name]["start_date"] = start
        final_output[Company_Name]["end_date"] = end
        final_output[Company_Name]["Company_Code"] = Company_Code

    final_data = {"_Source": final_output}
    # Writing Pickle file
    return final_data














###############################################################################################

r_forecast = importr("forecast")
stats = importr('stats')
tseries = importr('tseries')
pandas2ri.activate()
r_forecast_obj = importr("DemandForecasting")


###############################################################################################
raw_data_json = Constants.RAW_DATA_JSON #'/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/raw_data/json'
output_folder = Constants.OUTPUT_FOLDER #'/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/output_data'
log_folder    = Constants.LOG_FOLDER    #'/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/codes/logs'
k_value       = Constants.K
a = constituent_data()
company_data = a.get_topK_dict(k=k_value)
keys = company_data['Name'].keys()
Forecasting_horizon = 6


LOG_FORMAT = "%(levelname)s %(asctime)s - %(message)s"
logging.basicConfig(filename = "Forecasting_Logging_"+datetime.datetime.today().strftime("%m%d%Y")+'.log',level = logging.DEBUG, format = LOG_FORMAT, filemode='w')
logger = logging.getLogger()

#################################################################################################
local_start_time  = datetime.datetime.strptime("20160101","%Y%m%d")
local_end_time    = datetime.datetime.today()

tm_zone         = stock_time_zone()
zone_start_time = tm_zone.get_timezone_time(dt_time=local_start_time)
zone_end_time   = tm_zone.get_timezone_time(dt_time=local_end_time)


for key in keys:
    print('################################################################################################')
    company_name    = company_data['Name'][key].replace(" ", "_")
    company_symbol  = company_data['Symbol'][key]
    #Need to fix this.
    file_name = fnmatch.filter(os.listdir(raw_data_json),company_name +'_'+ '*' + '_Stock_data.json')[0]
    print("File picked is :",file_name)
    file_name = raw_data_json + '/' + file_name
    output_file_path = output_folder + '/' + company_name + "_Forecast_" + zone_end_time.strftime("%m%d%Y") + ".pkl"
    output_file_name = company_name + "_Forecast_" + zone_end_time.strftime("%m%d%Y") + ".pkl"
    #########


    if output_file_name in os.listdir(output_folder):
        print("Output Already exists!!")
        continue

    comp_obj = Company_data( company_name= company_name, company_symbol=company_symbol, raw_json_path = file_name, start_time=zone_start_time, end_time=zone_end_time)

    comp_obj.set_pandas_data_file()



    if comp_obj.data_problem:

        print("-------There is Problem with raw data:",company_name)
        continue




    try:
        final_data = run_model(comp_obj,Forecasting_horizon= Forecasting_horizon)
        with open(output_file_path, 'wb') as fp:
            pickle.dump(final_data, fp)
    except:
        print('====== Algorithm Did not work:',company_name)




