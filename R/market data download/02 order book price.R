# load library and source files
source("./R/api/util.R")
source("./R/api/binance_wrapper.R")

# get all symbol pairs
symbols_list     <- get_exchange_info()$symbols
symbol_pair      <- sapply(symbols_list, '[[', 1) %>% as.character()


# download market spot price data

while(TRUE){
  p1 = proc.time()
  # order_book with depth of a given symbol pair (symbol_pair can be NULL)
  order_book_df_list <- lapply(symbol_pair, function(x) get_orderbook_price_depth(x, 5))
  order_book_df      <- do.call("rbind", order_book_df_list)
  
  data.table::fwrite(as.data.frame(order_book_df),  append = T,
                     file = paste0("./data/market data/order_book.csv"))
  
  p2 = proc.time() - p1
  Sys.sleep(max((1 - p2[3]), 0)) #basically sleep for whatever is left of the second
}


