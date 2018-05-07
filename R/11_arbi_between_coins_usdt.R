# idea 1 how to find the arbitrage between coins 

# Assumption 

# 1: we only do 2 trades for a pair of coins (Simpliest and less commission)

# get all the coins that can be traded to USDT
symbol_can_be_traded_in_usd <- symbol_pair_df %>%
  filter(quoteAsset == "USDT") %>% 
  select(baseAsset) %>% .[,1]


main_f <- function(order_best_all){
  
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
  # =====================
  
  
  
  symbol_can_be_traded_in_usd_pair <- as.data.frame(t(combn(symbol_can_be_traded_in_usd,2)), 
                                                    stringsAsFactors = F) %>% 
    rename(coin_A  = V1, coin_B = V2) %>% 
    mutate(coin_O  = "USDT",
           pair_OA = paste0(coin_O, coin_A),
           pair_AB = paste0(coin_A, coin_B),
           pair_BO = paste0(coin_B, coin_O)) %>% 
    
    left_join(order_best_all %>% select(symbol_pair, askPrice, askQty) %>% 
                rename(OA_pair_askPrice = askPrice,
                       OA_pair_askQty   = askQty),
              by = c("pair_OA" = "symbol_pair")) %>% 
    
    left_join(order_best_all %>% select(symbol_pair, askPrice, askQty) %>% 
                rename(AB_pair_askPrice = askPrice,
                       AB_pair_askQty   = askQty),
              by = c("pair_AB" = "symbol_pair")) %>% 
    
    left_join(order_best_all %>% select(symbol_pair, bidPrice, bidQty) %>% 
                rename(BO_pair_bidPrice = bidPrice,
                       BO_pair_bidQty   = bidQty),
              by = c("pair_BO" = "symbol_pair")) %>% 
    
    mutate(result_100O = 100*OA_pair_askPrice*AB_pair_askPrice*BO_pair_bidPrice,
           profit_100O = result_100O-100*1.001) %>% 
    filter(!is.na(profit_100O)) %>% 
    mutate(time = Sys.time())
  
  
}

test <- function(){
  
  df <- main_f()
  
  data.table::fwrite(as.data.frame(df),  append = T,
                     file = paste0("11_arbi_between_coins_test1.csv"))
  print(Sys.time())
}


while(TRUE){
  p1 = proc.time()
  test()
  p2 = proc.time() - p1
  Sys.sleep(max((10 - p2[3]), 0)) #basically sleep for whatever is left of the second
}
