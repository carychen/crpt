from binance.client import Client
from sqlalchemy import create_engine

import pandas as pd
import time
import threading
import MySQLdb


engine = create_engine("mysql://user:passwd@host/schema")


client = Client('bDhFPg8QzGXzZGz8DHRCUHt1ByDIko5HokIPZNzCTfLfZehSs8f6ZdRPjSod1pJA', 'Gql50EECcJPj849aJvNazHyEfjORJCAX709r02fQmg4ykB2OpAtUfVCyADYqYWA')
info = client.get_exchange_info()['symbols']
pair_list = [item['symbol'] for item in info]


def depth_download(symbol):
  depth = client.get_order_book(symbol=symbol, limit = 20)
  bids = pd.DataFrame.from_dict(depth["bids"]).iloc[:, 0:2]
  bids.columns = ['bid_price', 'bid_qty']
  
  asks = pd.DataFrame.from_dict(depth["asks"]).iloc[:, 0:2]
  asks.columns = ['ask_price', 'ask_qty']
  
  all = pd.concat([asks, bids], axis =1)
  
  all['timestamp'] = time.time()
  all['symbol'] = symbol
  all['exchange'] = 'Binance'
  
  all.to_sql(name = symbol, con=engine, if_exists='append', index = False)

def depth_download_all():
  coin_list = pair_list
  map(depth_download, coin_list)

def depth_download_tick():
  threading.Timer(1, depth_download_tick).start()
  depth_download('BTCUSDT')
  
depth_download_tick()



