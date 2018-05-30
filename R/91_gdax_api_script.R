
transform_time_to_ISO_8601 <- function(time_point){
  as.POSIXlt(time_point, "UTC", "%Y-%m-%dT%H:%M:%S")}



# load api key and secret
credentials <- yaml.load_file(input = "./config.yaml")[["GDAX"]]

# all products
gdax_query(endpoint = '/products')

gdax_query(endpoint = '/time')

gdax_query(endpoint = '/products/BTC-USD/trades')

gdax_query(endpoint = '/products/BTC-USD/candles',
           params = list(end   = transform_time_to_ISO_8601(Sys.time()),
                         start = transform_time_to_ISO_8601(Sys.time() - 10000),
                         granularity = as.character(300)))
