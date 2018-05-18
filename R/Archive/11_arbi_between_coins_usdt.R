# idea 1 how to find the arbitrage between coins 

# Assumption 

# 1: we only do 2 trades for a pair of coins (Simpliest and less commission)

# get all the coins that can be traded to USDT
symbol_can_be_traded_in_usd <- symbol_pair_df %>%
  filter(quoteAsset == "USDT") %>% 
  select(baseAsset) %>% .[,1]


O_value <- 10
fee_rate <- 2*0.0005

main_f <- function(O_value){
  
  order_best_all <- get_best_order_all_pair() %>% 
    left_join(symbol_pair_df, by = "symbol_pair") %>% 
    mutate(original_symbol_pair = symbol_pair)
  
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
  # ====================
  
  symbol_can_be_traded_in_usd_pair <- as.data.frame(t(combn(symbol_can_be_traded_in_usd,2)), 
                                                    stringsAsFactors = F) %>% 
    rename(coin_A  = V1, coin_B = V2) %>% 
    mutate(coin_O  = "USDT",
           pair_OA = paste0(coin_O, coin_A),
           pair_AB = paste0(coin_A, coin_B),
           pair_BO = paste0(coin_B, coin_O)) %>% 
    
    left_join(order_best_all %>% select(symbol_pair, askPrice, askQty,
                                        original_symbol_pair, 
                                        minQty_filter_round,
                                        minQty_filter) %>% 
                rename(OA_pair_askPrice = askPrice,
                       OA_pair_askQty   = askQty,
                       OA_pair_original = original_symbol_pair,
                       OA_minQty_filter_round= minQty_filter_round,
                       OA_minQty_filter = minQty_filter),
              by = c("pair_OA" = "symbol_pair")) %>% 
    mutate(OA_Value_in_O = OA_pair_askQty/OA_pair_askPrice) %>% 
    
    left_join(order_best_all %>% select(symbol_pair, askPrice, askQty,
                                        original_symbol_pair, 
                                        minQty_filter_round,
                                        minQty_filter) %>% 
                rename(AB_pair_askPrice = askPrice,
                       AB_pair_askQty   = askQty,
                       AB_pair_original = original_symbol_pair,
                       AB_minQty_filter_round= minQty_filter_round,
                       AB_minQty_filter = minQty_filter),
              by = c("pair_AB" = "symbol_pair")) %>%
    
    left_join(order_best_all %>% select(symbol_pair, bidPrice, bidQty,
                                        original_symbol_pair, 
                                        minQty_filter_round,
                                        minQty_filter) %>% 
                rename(BO_pair_bidPrice = bidPrice,
                       BO_pair_bidQty   = bidQty,
                       BO_pair_original = original_symbol_pair,
                       BO_minQty_filter_round= minQty_filter_round,
                       BO_minQty_filter = minQty_filter),
              by = c("pair_BO" = "symbol_pair")) %>% 
    
    mutate(AB_Value_in_O = AB_pair_askQty*BO_pair_bidPrice,
           BO_Value_in_O = BO_pair_bidQty*BO_pair_bidPrice,
           AB_Side       = ifelse(AB_pair_original == pair_AB, "SELL","BUY"),
           
           OA_buy_quantity = O_value*OA_pair_askPrice,
           OA_buy_quantity_r = round(OA_buy_quantity, OA_minQty_filter_round),
           
           AB_trade_quatity = OA_buy_quantity_r*(1/(1+fee_rate))*AB_pair_askPrice,
           AB_trade_quatity_r = floor(AB_trade_quatity/AB_minQty_filter)*AB_minQty_filter,
           
           BO_trade_quatity = AB_trade_quatity_r*(1/(1+fee_rate)),
           BO_trade_quatity_r = floor(BO_trade_quatity/BO_minQty_filter)*BO_minQty_filter
           ) %>% 
    
    # This part is wrong, it's not the real quanity we are trading 
    # mutate(result_O = O_value*OA_pair_askPrice*AB_pair_askPrice*BO_pair_bidPrice,
    #        profit_O = result_O-O_value*1.003,
    #        profit_O_percentage = 100*profit_O/O_value) %>% 
    mutate(result_O = BO_trade_quatity_r*BO_pair_bidPrice*(1-fee_rate),
           profit_O = result_O-OA_buy_quantity_r/OA_pair_askPrice,
           profit_O_percentage = 100*profit_O/(OA_buy_quantity_r/OA_pair_askPrice)) %>% 
    
    filter(!is.na(profit_O),
           profit_O_percentage  > 0.002,
           OA_Value_in_O > 10*O_value, 
           AB_Value_in_O > 10*O_value,
           BO_Value_in_O > 10*O_value
           # ,
           # AB_minQty_filter_round >=3
           ) %>% 
    mutate(time = Sys.time()) %>% 
    arrange(-profit_O) %>% 
    head(1)
  
}

