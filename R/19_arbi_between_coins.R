# load library and source files
source("./R/api/util.R")
source("./R/api/binance_wrapper.R")

# load api key and secret
credentials <- yaml.load_file(input = "./config.yaml")[["Binance2"]]

# set trading parameters 
O_value <- 15
trading_fee_rate <- 2*0.0005

# Symbol list information: 
# It will be usefull to as join key, and with minimal filter information

symbols_list     <- get_exchange_info()$symbols
symbol_pair      <- sapply(symbols_list, '[[', 1)
baseAsset        <- sapply(symbols_list, '[[', 3)
quoteAsset       <- sapply(symbols_list, '[[', 5)
minQty_filter    <- as.numeric(sapply(sapply(sapply(symbols_list, '[', 9), 
                                             '[', 2), '[[', 2))

# minQty_filter and minQty_filter_round_digits are part of trade filters
symbol_pair_df   <- as.data.frame(cbind(symbol_pair, 
                                        baseAsset, quoteAsset,minQty_filter),
                                  stringsAsFactors = F) %>% 
  mutate(minQty_filter              = as.numeric(minQty_filter),
         minQty_filter_round_digits = log10(1/minQty_filter))

# We will use USDT as the our target coin -> Goal is to earn more USDT
# get all the coins that can be traded to USDT
symbol_can_be_traded_in_usd <- symbol_pair_df %>%
  filter(quoteAsset == "USDT") %>% 
  select(baseAsset) %>% .[,1] %>% combn(2) %>% 
  t() %>% 
  as.data.frame() %>% 
  transmute(baseAsset  = as.character(V1), 
         quoteAsset = as.character(V2)) 



# ==========================================================================

# For deciding whether trade or not
decide_trade_or_no <- function(O_value){
  
  # get all pairs best order book
  order_best_all <- get_best_order_all_pair() %>% 
      left_join(symbol_pair_df, by = "symbol_pair") %>% 
    select(symbol_pair,
           baseAsset, 
           quoteAsset,
           askPrice, askQty,
           bidPrice, bidQty,
           minQty_filter)
  
  # Generate all possible "coin flow" with USDT as start and USDT as end
  # O: USDT;
  # A: Sell USDT to buy A; Sell A to buy B;
  # B: Sell B to buy O; 
  decide_trade_df <- symbol_can_be_traded_in_usd %>% 
    mutate(coin_O  = "USDT",
           pair_OA = paste0(baseAsset, coin_O),
           pair_AB = paste0(quoteAsset, baseAsset),
           pair_BO = paste0(quoteAsset, coin_O)) %>% 
    left_join(order_best_all %>% select(symbol_pair,
                                        AB_pair_askPrice = askPrice,
                                        AB_pair_askQty   = askQty,
                                        AB_minQty_filter = minQty_filter),
              by = c("pair_AB" = "symbol_pair")) %>% 
    filter(!is.na(AB_pair_askPrice)) %>% 
    
    left_join(order_best_all %>% select(symbol_pair,
                                        OA_pair_askPrice = askPrice,
                                        OA_pair_askQty   = askQty,
                                        OA_minQty_filter = minQty_filter), 
              by = c("pair_OA" = "symbol_pair")) %>% 
    
    left_join(order_best_all %>% 
                select(symbol_pair,
                       BO_pair_bidPrice = bidPrice,
                       BO_pair_bidQty   = bidQty,
                       BO_minQty_filter = minQty_filter),
              by = c("pair_BO" = "symbol_pair")) %>% 
    
    mutate(OA_Value_in_O = OA_pair_askQty*OA_pair_askPrice,
           AB_Value_in_O = AB_pair_askQty*BO_pair_bidPrice,
           BO_Value_in_O = BO_pair_bidQty*BO_pair_bidPrice,
           
           OA_buy_quantity = O_value/OA_pair_askPrice,
           OA_buy_quantity_r = round(OA_buy_quantity/OA_minQty_filter, 0)*OA_minQty_filter,
           
           AB_trade_quatity = OA_buy_quantity_r*(1-trading_fee_rate)/AB_pair_askPrice,
           AB_trade_quatity_r = floor(AB_trade_quatity/AB_minQty_filter)*AB_minQty_filter,
           
           BO_trade_quatity = AB_trade_quatity_r*(1-trading_fee_rate),
           BO_trade_quatity_r = floor(BO_trade_quatity/BO_minQty_filter)*BO_minQty_filter
    ) %>% 
    mutate(result_O = AB_trade_quatity*(1-trading_fee_rate)^2*BO_pair_bidPrice,
           profit_O = result_O-OA_buy_quantity_r*OA_pair_askPrice,
           profit_O_percentage = 100*profit_O/(OA_buy_quantity_r*OA_pair_askPrice))

    data.table::fwrite(as.data.frame(decide_trade_df),  append = T,
                       file = paste0("trade_results.csv"))
    
    decide_trade_df %>% filter(profit_O_percentage  > 0.0015,
           OA_Value_in_O > 10*O_value,
           AB_Value_in_O > 10*O_value,
           BO_Value_in_O > 10*O_value) %>% 
    mutate(time = Sys.time()) %>% 
    arrange(-profit_O) %>% 
    head(1)
}


# Trade
trade_main <- function(O_value = 15){
  
  price_qty_df <- decide_trade_or_no(O_value)

  # Buy order at market price
  if(nrow(price_qty_df) == 1 ){
    price_qty_df$OA_pair_original
    price_qty_df$OA_pair_askPrice
    
    # Buy order at market price
    # OA_buy_result <- place_order_limit(symbol   = price_qty_df$OA_pair_original,
    #                                    side     = "BUY",
    #                                    quantity = price_qty_df$OA_buy_quantity_r,
    #                                    price    = 1/price_qty_df$OA_pair_askPrice,
    #                                    timeInForce = "GTC")
    OA_buy_result <- place_order_market(symbol   = price_qty_df$OA_pair_original,
                                       side     = "BUY",
                                       quantity = price_qty_df$OA_buy_quantity_r)
    # if(content(OA_buy_result)$status == "FILLED"){
    
    # Buy order at market price
    # AB_buy_result <- place_order_limit(symbol   = price_qty_df$AB_pair_original,
    #                                    side     = price_qty_df$AB_Side,
    #                                    quantity = price_qty_df$AB_trade_quatity_r,
    #                                    price    = price_qty_df$AB_pair_askPrice,
    #                                    timeInForce = "GTC")
    AB_buy_result <- place_order_market(symbol   = price_qty_df$AB_pair_original,
                                        side     = "BUY",
                                        quantity = price_qty_df$AB_trade_quatity_r)
    # content(AB_buy_result)
    
    # Sell order at market price
    # BO_buy_result <- place_order_limit(symbol   = price_qty_df$OB_pair_original,
    #                                    side     = "SELL",
    #                                    quantity = price_qty_df$BO_trade_quatity_r,
    #                                    price    = price_qty_df$BO_pair_bidPrice,
    #                                    timeInForce = "GTC")
    BO_buy_result <- place_order_limit(symbol   = price_qty_df$OB_pair_original,
                                       side     = "SELL",
                                       quantity = price_qty_df$BO_trade_quatity_r)
    # content(BO_sell_result)
    
    print(paste("Traded at", Sys.time()))
    # data.table::fwrite(as.data.frame(price_qty_df),  append = T,
    #                    file = paste0("trade_results.csv"))
    browser()
    
    # }
    
    
  }  else{print(paste("No trigger for Trading at", Sys.time()))}
  
}



while(TRUE){
  p1 = proc.time()
  trade_main(20)
  p2 = proc.time() - p1
  Sys.sleep(max((3 - p2[3]), 0)) #basically sleep for whatever is left of the second
}

