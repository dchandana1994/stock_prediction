import pickle
import json,fnmatch
from  codes.Companies import constituent_data,ApiDetails,Company_data,stock_time_zone
import datetime
import os
import pandas as pd
import numpy as np
from codes.Commons import Constants



raw_data_json     = Constants.RAW_DATA_JSON #'/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/raw_data/json'
output_folder     = Constants.OUTPUT_FOLDER #'/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/output_data'
output_csv_folder = '/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/output_data/csv_10092017'

log_folder        = Constants.LOG_FOLDER  #'/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/codes/logs'

####################################################



a = constituent_data()
company_details = a.get_topK_dict(k=10)
keys = company_details['Name'].keys()
#####################################
local_start_time  = datetime.datetime.strptime("20160101","%Y%m%d")
local_end_time    = datetime.datetime.today()

tm_zone         = stock_time_zone()
zone_start_time = tm_zone.get_timezone_time(dt_time=local_start_time)
zone_end_time   = tm_zone.get_timezone_time(dt_time=local_end_time)

no_of_days      = 30


######################################

item_info = ['Open','Close','High','Low']

for key in keys:
    print('################################################################################################')

    company_name    = company_details['Name'][key].replace(" ", "_")
    company_symbol  = company_details['Symbol'][key]
    file_paths = fnmatch.filter(os.listdir(output_folder),company_name +'_Forecast_'+ zone_end_time.strftime('%m%d%Y') +'*' + '.pkl')
    print(company_name)
    ###################################################################################################
    if file_paths!=[]:
        file_path = file_paths[0]
    else:
        print('------The File Doesnt exist')
        continue


    output_csv_file  = company_name +'_Forecast_'+ zone_end_time.strftime('%m%d%Y') + '.csv'
    ###################################################################################################
    file_name = output_folder + '/' + file_path

    with open(file_name,'rb') as fp:
        data = pickle.load(fp)

    ###################################################################################################

    pd_list = []
    for item in item_info:
        forecast_horizon  = data['_Source'][company_name]['Forecast_Horizon']
        forecast_data     = data['_Source'][company_name][item]['Forecast']
        uncertainity      = data['_Source'][company_name][item]['uncertainity']

        pd_forecast_data  = pd.DataFrame(forecast_data)
        pd_forecast_slice = pd_forecast_data[-no_of_days:]
        list_uncertainity = [np.nan] * (no_of_days - forecast_horizon)
        list_uncertainity +=uncertainity
        pd_forecast_slice['Uncertainity'] = list_uncertainity

        pd_forecast_slice.columns = list( map(lambda x: item+'_'+x, pd_forecast_slice.columns) )
        pd_list +=[pd_forecast_slice]


    pd_out = pd.concat(pd_list,axis=1)
    pd_out.index.name = "Date"
    pd_out.to_csv(output_csv_folder+'/'+output_csv_file)




    print('djkfjdkjfd')


    print('dkfjdkjfkdj')


