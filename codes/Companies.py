import pandas as pd
#import urllib.request
import urllib3
import json
import datetime,pytz
from codes.Commons import Constants



#This data is for getting the company Names in S&P
class constituent_data:
    def __init__(self):
        self.description = 'This class provides the top S&P Company details.'
        raw_data_folder = Constants.RAW_DATA_JSON #"/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/raw_data/"
        #output_folder = "/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/output_data/"
        file_name = Constants.COMPANY_FILE

        self.company_names = pd.read_csv( file_name )

    def get_topK_dict(self, k=10):
        return self.company_names[:k].to_dict()




#This class provides details of API for feteching the data.
class ApiDetails():
    def __init__(self,api_key = 'UGJN261OWZA9KCVJ'):
        self.api_key = api_key
        self.source  ="Alpha Vantage"
        self.source_link = "https://www.alphavantage.co/"
        self.description = "This class provides details API details for downloading the Data"


    def intraDay_data_link(self, companySymbol, full_data=False):
        '''
        Link details Compact  : https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=1min&apikey=demo
        Link Details Full     : https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=15min&outputsize=full&apikey=demo
        '''
        if not full_data:
            link_address = 'https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol='+companySymbol+'&interval=1min&apikey='+ self.api_key
        else:
            link_address =  'https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol='+companySymbol+'&interval=15min&outputsize=full&apikey='+self.api_key

        return link_address


    def daily_data_link(self, companySymbol, full_data=False):
        '''
        Compact Data : https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=MSFT&apikey=demo
        Full Data    : https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=MSFT&outputsize=full&apikey=demo
        '''

        if not full_data:
            link_address = 'https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=' + companySymbol + '&apikey=' + self.api_key
        else:
            link_address = 'https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=' + companySymbol + '&outputsize=full&apikey=' + self.api_key

        return link_address


    # def get_data(self,companySymbol,full_data=False):
    #
    #     link = self.daily_data_link(companySymbol=companySymbol,full_data=full_data)
    #




#This class provides the data from local or our mongoDB.
class Company_data():
    def __init__(self, company_name, company_symbol, start_time, end_time, raw_json_path = None ):
        self.raw_data_path       = raw_json_path
        self.company_name        = company_name
        self.company_symbol      = company_symbol
        self.start_time          = start_time
        self.end_time            = end_time
        self.description         = "This class provides the data in pandas format from flat files like json or mongoDB"


    def set_pandas_data_file(self, format = "csv"):
        '''

        :param company_name:
        :param company_symbol:
        :param start_date:
        :param end_date:
        :param format:
        :return:
        '''
        with open(self.raw_data_path, 'r') as fp:
            data = json.load(fp)


        if 'Time Series (Daily)' not in data['_Source']:
            print("There is problem in the data")
            self.data_problem = True

        else:
            self.data_problem = False
            df_data         = pd.DataFrame(data['_Source']['Time Series (Daily)']).transpose()
            self.meta_data  = data['_Source']['Meta Data']

            df_data.columns = ['Open','High','Low','Close','Volume']
            for col in df_data.columns:
                df_data[col]  = df_data[col].astype('float64')


            df_data.index   = df_data.index.to_datetime()
            df_data['Time_Stamp'] = df_data.index


            self.company_data = df_data[(df_data['Time_Stamp']>= self.start_time)  & (df_data['Time_Stamp'] <= self.end_time)]
            self.actual_start_time = min(self.company_data['Time_Stamp'])
            self.actual_end_teime  = max(self.company_data['Time_Stamp'])
            del self.company_data['Time_Stamp']



    def get_company_data(self):
        return self.company_data




    def get_pandas_data_mongo(self, start_time, end_time, format = "csv"):
        '''

        :param company_name:
        :param company_symbol:
        :param start_date:
        :param end_date:
        :param format:
        :return:
        '''



class stock_time_zone(object):
    def __init__(self,time_zone = 'US/Eastern'):
        self.time_zone   = time_zone
        self.description = "This class provides different time zones"

    def get_timezone_time(self,dt_time = datetime.datetime.today()):
        set_timezone  = pytz.timezone(self.time_zone)
        time_timezone = set_timezone.localize(dt_time)
        return time_timezone




class fetch_data():
    def __init__(self):
        self.description = 'This class provides a wrapper for urllib3 for fetching data from different sources'


    def get_alpha_data(self,http_link):
        http = urllib3.PoolManager()
        try:
            data_obj  = http.request('GET',http_link)
            json_data = json.loads(data_obj.data.decode('utf-8'))
            return json_data


            # source_data = {'_Source':json_data}
            # print("Writing Data of Company:",company_name)
            # with open(file_name,'w') as fp:
            #     json.dump(source_data,fp)


        except:
            print('@@@@@@@@@There is problem in fetching data, Link didnot work')
            return {}
