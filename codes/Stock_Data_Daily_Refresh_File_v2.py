from Companies import ApiDetails,stock_time_zone,constituent_data,fetch_data
import datetime,pytz
import urllib3
import fnmatch,os
import json
import pandas_market_calendars as mcal
from collections import defaultdict





# Raw_data details for
raw_data_json = '/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/raw_data/json'




#############################

local_start_time  = datetime.datetime.strptime("20160101","%Y%m%d")
local_end_time    = datetime.datetime.today()

tm_zone         = stock_time_zone()
zone_start_time = tm_zone.get_timezone_time(dt_time=local_start_time)
zone_end_time   = tm_zone.get_timezone_time(dt_time=local_end_time)
zone_today_str  = zone_end_time.strftime('%m%d%Y')

##############################






api_d    = ApiDetails()
s_p_data = constituent_data()
get_data = fetch_data()
company_details = s_p_data.get_topK_dict(k=500)

#These are the compaany names.
keys = company_details['Name'].keys()

http = urllib3.PoolManager()

############################################
count =1
for key in keys:

    print('################################################################################################')
    print(count)
    company_name = company_details['Name'][key].replace(" ", "_")
    company_symbol = company_details['Symbol'][key]
    print('This company Getting the Data:',company_name)

    file_names = fnmatch.filter(os.listdir(raw_data_json), company_name + '_' + '*' + '_Stock_data.json')


    if len(file_names)==1:
        old_file_name =  raw_data_json + '/' + file_names[0]
    else:
        print('There are more files with different dates, Not doing anything   -----> Conflict')
        continue

    new_file_name = raw_data_json + '/' + company_name+'_'+zone_today_str+'_Stock_data.json'



    if old_file_name==new_file_name:
        count+=1
        print("!!!!!!!!The File Already Exists")

        continue


    with open(old_file_name,'r') as fp:
        data = json.load(fp)


    #########################################################################################



    if 'Time Series (Daily)' not in data['_Source']:
        count+=1
        print("========COMPANY OLD DATA PROBLEM:",company_name,'    ',company_symbol)
        continue



    #data['_Source']['Time Series (Daily)'] = defaultdict(dict)
    time_series_old = data['_Source']['Time Series (Daily)']


    fun_strp_time = lambda x: datetime.datetime.strptime(x, '%Y-%m-%d')



    dt_timestamps_old = map(fun_strp_time, time_series_old.keys())
    max_timestamp_old = max(dt_timestamps_old)

    # Need to write the case for the data if not updated before 100 days!!!, Kindly check this piece of code.

    #Getting data from API for last 100 days.

    link      = api_d.daily_data_link(companySymbol=company_symbol,full_data=False)
    new_data  = get_data.get_alpha_data(http_link= link)

    if 'Time Series (Daily)' not in new_data:
        print('--------PROBLEM IN FETECHING DATA')
        count+=1
        continue

    time_series_new = new_data['Time Series (Daily)']

    dt_timestamps_new = map(fun_strp_time, time_series_new.keys())
    max_timestamp_new = max(dt_timestamps_new)

    ######

    if max_timestamp_new==max_timestamp_old:

        # Need to change the file name
        print("There is no new data point")
        print("Just Updating the meta Data")
        data['_Source']['Meta Data'] = new_data['Meta Data']
        with open(new_file_name,'w') as fp:
            json.dump(data,fp)

        if new_file_name!=old_file_name:
            os.remove(old_file_name)



    else:

        new_date_str   = max_timestamp_new.strftime('%Y-%m-%d')
        old_date_str   = max_timestamp_old.strftime('%Y-%m-%d')
        nyse           = mcal.get_calendar('NYSE')
        date_range     = nyse.schedule(start_date=old_date_str,end_date=new_date_str)
        datetime_index = mcal.date_range(date_range,frequency='1D')
        datetime_index = datetime_index.tz_convert(tm_zone.time_zone)
        fun_time_str   = lambda x : x.strftime('%Y-%m-%d')
        time_str_list  = list(map(fun_time_str,datetime_index.to_pydatetime()))


        data['_Source']['Meta Data'] = new_data['Meta Data']
        for time_str in time_str_list:
            data['_Source']['Time Series (Daily)'][time_str] = new_data['Time Series (Daily)'][time_str]



        with open(new_file_name,'w') as fp:
            json.dump(data,fp)


        print('Writing the file is completed')
        os.remove(old_file_name)

    count+=1