from pymongo import MongoClient
import json



client = MongoClient()
db     = client.CompanyStockData



raw_data_json = '/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/raw_data/json'

file_name = '3M_Company_10072017_Stock_data.json'



with open(raw_data_json+'/'+file_name,'r') as fp:
    data = json.load(fp)



meta_data_col    = 'Meta data'
series_data_col  = 'Time Series (Daily)'












print('dkfldkfldkfkdl')















