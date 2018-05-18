source("./R/api/binance_wrapper.R")

library(dplyr)

# symbol list. Transfer the list to a useful dataframe
symbols_list     <- get_exchange_info()$symbols
symbol_pair      <- sapply(symbols_list, '[[', 1)
baseAsset        <- sapply(symbols_list, '[[', 3)
quoteAsset       <- sapply(symbols_list, '[[', 5)
minQty_filter    <- as.numeric(sapply(sapply(sapply(symbols_list, '[', 9), 
                           '[', 2), '[[', 2))

symbol_pair_df   <- as.data.frame(cbind(symbol_pair, 
                          baseAsset, quoteAsset,minQty_filter),
                          stringsAsFactors = F) %>% 
  mutate(minQty_filter = as.numeric(minQty_filter),
         minQty_filter_round = log10(1/minQty_filter))

# order book for each pair of coins
get_TOP_order_each_pair <- function(symbol_pair){
  
  order_list   <- get_order_book(symbol_pair)
  
  order_bids_price   <- sapply(order_list$bids, '[[', 1) 
  order_bids_qty     <- sapply(order_list$bids, '[[', 2) 
  order_asks_price   <- sapply(order_list$asks, '[[', 1) 
  order_asks_qty     <- sapply(order_list$asks, '[[', 2)
  
  order_book   <- as.data.frame(cbind(lastUpdateId = order_list$lastUpdateId,
                                      order_bids_price, order_bids_qty,
                                      order_asks_price, order_asks_qty),
                                stringsAsFactors = F) 
  order_book
  
}


order_best_all <- get_best_order_all_pair() %>% 
  left_join(symbol_pair_df, by = "symbol_pair")

order_best_all_reverse <- order_best_all %>% 
  mutate(temp_v      = baseAsset, 
         baseAsset   = quoteAsset,
         quoteAsset  = temp_v,
         symbol_pair = paste0(baseAsset, quoteAsset),
         temp_v      = bidPrice,
         bidPrice    = 1/askPrice,
         askPrice    = 1/temp_v,
         temp_v      = bidQty,
         bidQty      = askQty,
         askQty      = temp_v) %>% 
  select(colnames(order_best_all))

order_best_all <- order_best_all %>% 
  bind_rows(order_best_all_reverse)
