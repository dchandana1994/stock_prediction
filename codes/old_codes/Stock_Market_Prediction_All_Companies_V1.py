
# coding: utf-8

# In[1]:

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
import numpy as np
import scipy as sp
from sklearn import linear_model
from yahoo_finance import Share
import pandas_datareader as web
from pandas.tseries.offsets import BDay
from matplotlib.dates import DateFormatter, WeekdayLocator,    DayLocator, MONDAY
from matplotlib.finance import candlestick_ohlc
import pickle
import os
from Companies import constituent_data
##################################################################################################################

r_forecast = importr("forecast")
stats = importr('stats')
tseries = importr('tseries')
pandas2ri.activate()
r_forecast_obj = importr("DemandForecasting")


# In[2]:

raw_data_folder = "/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/raw_data/"
output_folder = "/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/output_data/"
#Number of Business Days to be forecasted. 
Forecasting_horizon=10
start = datetime.datetime(2016,1,1)
end = datetime.date.today()
start_date_str = start.strftime("%m%d%Y")
end_date_str = end.strftime("%m%d%Y")
topK = 2


# In[4]:

Comp_data= constituent_data()
companies_dict= Comp_data.get_topK_dict(k=topK)
indices = companies_dict['Symbol'].keys()


# In[5]:




# In[6]:

for id_no in indices:
    #Declaring Company variables.
    Company_Name   = companies_dict['Name'][id_no].replace(" ","_")
    Company_Code   = companies_dict['Symbol'][id_no]
    Company_Sector = companies_dict['Sector'][id_no]
    file_path = raw_data_folder + Company_Name +"_" + start_date_str + "_" +"Stock_Data.csv"
    today_date = datetime.datetime.today()
    print("##############################################################################################")
    print("Working on:",Company_Name)
    #########################################################################
    #This piece of code checks the data exists (or) not
    if os.path.isfile(file_path):
        csv_data = pd.read_csv(file_path)
        csv_data.index = pd.to_datetime(csv_data['Date'])
        del csv_data['Date']
        end_time = max(csv_data.index)
        query_start_date = datetime.datetime.strptime((end_time + datetime.timedelta(days=1)).strftime('%Y-%m-%d'),"%Y-%m-%d")
        query_end_date = today_date
        new_data = web.DataReader(Company_Code,"yahoo",query_start_date,query_end_date)
        temp_data = pd.concat([csv_data,new_data])
        stock_data = temp_data.groupby(temp_data.index).first()    
    else:
        stock_data = web.DataReader(Company_Code, "yahoo", start, end)

    stock_data.to_csv(file_path)
    #########################################################################
    regress = linear_model.LinearRegression(fit_intercept=True)
    failed_combination={}
    list_columns = ["Close","Open","High","Low"]
    final_output = {Company_Name:{}}
    
    ##########################################################################
    
    print("Buliding Model for :",Company_Name)
    for typeof in list_columns:
        print("typeof:",typeof)

        final_output[Company_Name][typeof] = {}
        comb_df = stock_data[[typeof]]
        comb_df['Time_Line']= comb_df.index
        comb_df=comb_df.rename(columns={typeof:'Frequency'})
        comb_df=comb_df.reset_index(drop=True)
        comb_df['index']=comb_df.index
        comb_df['Year']=comb_df.Time_Line.dt.year
        comb_df['month']=comb_df.Time_Line.dt.month

        ##############
        # This is were the pandas dataframe is converted into 'r' object. 
        r_skill_print_freq = pandas2ri.py2ri(comb_df)
        # Forecasting_horizon let you forecast from the end_date  till end_date + Forecasting_horizon. 


        r_prediction_period = ro.IntVector([Forecasting_horizon])


        # In[112]:


        r_forecasted_values_stl = r_forecast_obj.hcl_forecasting_stl(r_skill_print_freq, r_prediction_period)
        forecasted_values_stl = r_forecasted_values_stl.rx()
        r_forecasted_values = list(forecasted_values_stl[8])
        for n_temp in range(0,Forecasting_horizon):
            r_forecasted_values.append(forecasted_values_stl[3][n_temp])
        for temp_n in range(0,len(r_forecasted_values)):
            if r_forecasted_values[temp_n] <0:
                r_forecasted_values[temp_n] = 0 




        ##########

        #Total number of actual Data points 
        length_actual= comb_df.shape[0]
        # Total Length of Forecasted values. 
        length_forecast = len(r_forecasted_values)
        # Total Length of predictions. 
        no_of_predictions=length_forecast

        rng = pd.date_range(start, periods=no_of_predictions, freq='M')
        ts = rng.to_period()
        rng = ts.to_timestamp()


        actual_time_series = pd.Series(np.array(comb_df.Frequency), index=comb_df.Time_Line)
        pred_time_series = pd.Series(np.array(r_forecasted_values), index=rng)


        error = comb_df.Frequency[3:] - r_forecasted_values[3:length_actual]


        MAPE = np.abs(error/comb_df.Frequency[3:])
        MAPE = np.mean(MAPE) 
        Uncertainity_measurement = []
        Error_variance = np.var(error)
        for i_temp in range(0,Forecasting_horizon):
            Uncertainity_measurement.append(np.sqrt((i_temp+1)*Error_variance))

        actual_df = actual_time_series.to_frame()
        actual_df.columns = ["Actual"]
        time_indices = actual_df.index


        forecast_time_list = []
        for i in range(1,Forecasting_horizon+1):
            forecast_time_list +=[end+BDay(i)]



        forecast_DatetimeIndex = pd.DatetimeIndex(forecast_time_list)



        final_forecast_DTIndex = time_indices.append(forecast_DatetimeIndex)



        forecasted_df = pd.DataFrame(data={"Forecasted": r_forecasted_values} , index=final_forecast_DTIndex)


        final_combine_data = pd.concat([actual_df,forecasted_df], axis=1)

        final_output[Company_Name][typeof]["Forecast"] = final_combine_data.to_dict()
        final_output[Company_Name][typeof]["MAPE"] = MAPE
        final_output[Company_Name][typeof]["uncertainity"] = Uncertainity_measurement
        final_output[Company_Name]["Forecast_Horizon"] = Forecasting_horizon
        final_output[Company_Name]["start_date"] = start
        final_output[Company_Name]["end_date"] = end
        final_output[Company_Name]["Company_Code"] = Company_Code
    
    final_data = {"_Source":final_output}
    #Writing Pickle file 
    with open(output_folder+Company_Name+"_Forecast_"+datetime.datetime.today().strftime("%m_%d_%Y")+".pkl",'wb') as fp:
        pickle.dump(final_data,fp)


# ##################################################################################################################
# def pandas_candlestick_ohlc(dat, stick = "day", otherseries = None):
#     """
#     :param dat: pandas DataFrame object with datetime64 index, and float columns "Open", "High", "Low", and "Close", likely created via DataReader from "yahoo"
#     :param stick: A string or number indicating the period of time covered by a single candlestick. Valid string inputs include "day", "week", "month", and "year", ("day" default), and any numeric input indicates the number of trading days included in a period
#     :param otherseries: An iterable that will be coerced into a list, containing the columns of dat that hold other series to be plotted as lines
#
#     This will show a Japanese candlestick plot for stock data stored in dat, also plotting other series if passed.
#     """
#     mondays = WeekdayLocator(MONDAY)        # major ticks on the mondays
#     alldays = DayLocator()              # minor ticks on the days
#     dayFormatter = DateFormatter('%d')      # e.g., 12
#
#     # Create a new DataFrame which includes OHLC data for each period specified by stick input
#     transdat = dat.loc[:,["Open", "High", "Low", "Close"]]
#     if (type(stick) == str):
#         if stick == "day":
#             plotdat = transdat
#             stick = 1 # Used for plotting
#         elif stick in ["week", "month", "year"]:
#             if stick == "week":
#                 transdat["week"] = pd.to_datetime(transdat.index).map(lambda x: x.isocalendar()[1]) # Identify weeks
#             elif stick == "month":
#                 transdat["month"] = pd.to_datetime(transdat.index).map(lambda x: x.month) # Identify months
#             transdat["year"] = pd.to_datetime(transdat.index).map(lambda x: x.isocalendar()[0]) # Identify years
#             grouped = transdat.groupby(list(set(["year",stick]))) # Group by year and other appropriate variable
#             plotdat = pd.DataFrame({"Open": [], "High": [], "Low": [], "Close": []}) # Create empty data frame containing what will be plotted
#             for name, group in grouped:
#                 plotdat = plotdat.append(pd.DataFrame({"Open": group.iloc[0,0],
#                                             "High": max(group.High),
#                                             "Low": min(group.Low),
#                                             "Close": group.iloc[-1,3]},
#                                            index = [group.index[0]]))
#             if stick == "week": stick = 5
#             elif stick == "month": stick = 30
#             elif stick == "year": stick = 365
#
#     elif (type(stick) == int and stick >= 1):
#         transdat["stick"] = [np.floor(i / stick) for i in range(len(transdat.index))]
#         grouped = transdat.groupby("stick")
#         plotdat = pd.DataFrame({"Open": [], "High": [], "Low": [], "Close": []}) # Create empty data frame containing what will be plotted
#         for name, group in grouped:
#             plotdat = plotdat.append(pd.DataFrame({"Open": group.iloc[0,0],
#                                         "High": max(group.High),
#                                         "Low": min(group.Low),
#                                         "Close": group.iloc[-1,3]},
#                                        index = [group.index[0]]))
#
#     else:
#         raise ValueError('Valid inputs to argument "stick" include the strings "day", "week", "month", "year", or a positive integer')
#
#
#     # Set plot parameters, including the axis object ax used for plotting
#     fig, ax = plt.subplots()
#     fig.subplots_adjust(bottom=0.2)
#     if plotdat.index[-1] - plotdat.index[0] < pd.Timedelta('730 days'):
#         weekFormatter = DateFormatter('%b %d')  # e.g., Jan 12
#         ax.xaxis.set_major_locator(mondays)
#         ax.xaxis.set_minor_locator(alldays)
#     else:
#         weekFormatter = DateFormatter('%b %d, %Y')
#     ax.xaxis.set_major_formatter(weekFormatter)
#
#     ax.grid(True)
#
#     # Create the candelstick chart
#     candlestick_ohlc(ax, list(zip(list(date2num(plotdat.index.tolist())), plotdat["Open"].tolist(), plotdat["High"].tolist(),
#                       plotdat["Low"].tolist(), plotdat["Close"].tolist())),
#                       colorup = "black", colordown = "red", width = stick * .4)
#
#     # Plot other series (such as moving averages) as lines
#     if otherseries != None:
#         if type(otherseries) != list:
#             otherseries = [otherseries]
#         dat.loc[:,otherseries].plot(ax = ax, lw = 1.3, grid = True)
#
#     ax.xaxis_date()
#     ax.autoscale_view()
#     plt.setp(plt.gca().get_xticklabels(), rotation=45, horizontalalignment='right')
#
#     plt.show()


# In[ ]:
#
# import matplotlib.pyplot as plt   # Import matplotlib
# # This line is necessary for the plot to appear in a Jupyter notebook
# get_ipython().magic(u'matplotlib inline')
# # Control the default size of figures in this Jupyter notebook
# get_ipython().magic(u'pylab inline')
# pylab.rcParams['figure.figsize'] = (15, 9)   # Change the size of plots
#
#
# stock_data["Open"].plot()
#
# with open(output_folder+Company_Name+"_Forecast_"+datetime.datetime.today().strftime("%m_%d_%Y")+".pkl",'rb') as fp:
#     dummy_var = pickle.load(fp)
#
#
# # In[ ]:
#
# # For Displaying Data
# pd.DataFrame(dummy_var["_Source"][Company_Name]['Open']['Forecast'])[:12]
#
#
# # In[ ]:
#
# # For Displaying last six Data points
# pd.DataFrame(dummy_var["_Source"][Company_Name]['Close']['Forecast'])[-6:]
#
# # For Displaying first six Data points
# #pd.DataFrame(dummy_var["_Source"][Company_Name]['Close']['Forecast'])[:6]
#
#
# # In[ ]:
#
# # For Displaying Data
# pd.DataFrame(dummy_var["_Source"][Company_Name]['High']['Forecast'])[-6:]
#
#
# # In[ ]:
#
# # For Displaying Data
# pd.DataFrame(dummy_var["_Source"][Company_Name]['Low']['Forecast'])[-4:]
#
#
# # In[ ]:
#
# close_forecast = pd.DataFrame(final_output[Company_Name]["Close"]["Forecast"])
# close_forecast.columns = ["Close_Actual","Close_Forecast"]
# open_forecast = pd.DataFrame(final_output[Company_Name]["Open"]["Forecast"])
# open_forecast.columns = ["Open_Actual","Open_Forecast"]
# high_forecast = pd.DataFrame(final_output[Company_Name]["High"]["Forecast"])
# high_forecast.columns = ["High_Actual","High_Forecast"]
# low_forecast = pd.DataFrame(final_output[Company_Name]["Low"]["Forecast"])
# low_forecast.columns = ["Low_Actual","Low_Forecast"]
#
#
# # In[ ]:
#
# all_data = pd.concat([open_forecast,close_forecast,high_forecast,low_forecast],axis=1)
#
#
# # In[ ]:
#
# forecast_all_data = all_data[["Open_Forecast","Close_Forecast","High_Forecast","Low_Forecast"]]
# forecast_all_data.columns =  ["Open","Close","High","Low"]
#
#
# # In[ ]:
#
# # Plotting Candle Plot for forecasted Data
# pandas_candlestick_ohlc(forecast_all_data[-10:])
#
#
# # In[ ]:
#
# forecast_all_data[-12:].to_csv('Temp_result.csv')
#
#
# # In[ ]:
#
# dummy_var["_Source"][Company_Name]['Open'].keys()
#
#
# # In[ ]:
#
# dummy_var["_Source"][Company_Name]['Open']['uncertainity']
#
#
# # In[ ]:
#
#
#
