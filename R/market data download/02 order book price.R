# load library and source files
source("./R/api/util.R")
source("./R/api/binance_wrapper.R")

# get all symbol pairs
symbols_list     <- get_exchange_info()$symbols
symbol_pair      <- sapply(symbols_list, '[[', 1) %>% as.character()

# order_book with depth of a given symbol pair (symbol_pair can be NULL)
sapply(symbol_pair[1:3], function(x) get_orderbook_price_depth(x, 5))
# download market spot price data

while(TRUE){
  p1 = proc.time()
  df <- get_spot_price() %>% 
    mutate(date_time = Sys.time())
  
  data.table::fwrite(as.data.frame(df),  append = T,
                     file = paste0("./data/market data/order_book.csv"))
  
  p2 = proc.time() - p1
  Sys.sleep(max((1 - p2[3]), 0)) #basically sleep for whatever is left of the second
}


