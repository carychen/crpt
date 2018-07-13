# load library and source files
source("./R/api/util.R")
source("./R/api/binance_wrapper.R")

# load api key and secret
credentials <- yaml.load_file(input = "./config.yaml")[["Binance"]]

# set trading parameters 
O_value <- 15
trading_fee_rate <- 0.0005

# Symbol list information: 
# It will be usefull to as join key, and with minimal filter information
# TODO: potentially can be simplified by using simple data structure or query useful vars
symbols_list     <- get_exchange_info()$symbols
symbol_pair      <- sapply(symbols_list, '[[', 1)
baseAsset        <- sapply(symbols_list, '[[', 3)
quoteAsset       <- sapply(symbols_list, '[[', 5)
minQty_filter    <- as.numeric(sapply(sapply(sapply(symbols_list, '[', 9), 
                                             '[', 2), '[[', 2))
minNotation    <- as.numeric(sapply(sapply(sapply(symbols_list, '[', 9), 
                                           '[', 3), '[[', 2))

# minQty_filter and minQty_filter_round_digits are part of trade filters
symbol_pair_df   <- as.data.frame(cbind(symbol_pair, baseAsset, 
                                        quoteAsset,minQty_filter,
                                        minNotation),
                                  stringsAsFactors = F) %>% 
  mutate(minQty_filter              = as.numeric(minQty_filter),
         minQty_filter_round_digits = log10(1/minQty_filter),
         minNotation                = as.numeric(minNotation))

# We will use USDT as the our target coin -> Goal is to earn more USDT
# get all the coins that can be traded to USDT
symbol_can_be_traded_in_usd <- symbol_pair_df %>%
  filter(quoteAsset == "USDT") %>% 
  select(baseAsset) %>% .[,1] %>% combn(2) %>% 
  t() %>% 
  as.data.frame() %>% 
  transmute(baseAsset  = as.character(V1), 
            quoteAsset = as.character(V2))

symbol_can_be_traded_in_usd <- symbol_can_be_traded_in_usd %>% 
  mutate(temp_v     = baseAsset,
         baseAsset  = quoteAsset,
         quoteAsset = temp_v) %>% 
  bind_rows(symbol_can_be_traded_in_usd) %>% 
  mutate(coin_O  = "USDT",
         pair_OA = paste0(baseAsset, coin_O),
         pair_AB = paste0(quoteAsset, baseAsset),
         pair_BO = paste0(quoteAsset, coin_O)) %>% 
  select(-temp_v)
# ==========================================================================

# For deciding whether trade or not
decide_trade_or_no <- function(O_value){
  #browser()
  # get all pairs best order book
  order_best_all <- get_best_order_all_pair() %>%
    left_join(symbol_pair_df, by = "symbol_pair") %>% 
    mutate(original_symbol_pair = symbol_pair)
  
  order_best_all_reverse <- order_best_all %>% 
    mutate(temp_v      = baseAsset, 
           baseAsset   = quoteAsset,
           quoteAsset  = temp_v,
           symbol_pair = paste0(baseAsset, quoteAsset),
           original_symbol_pair = paste0(quoteAsset, baseAsset)) %>% 
    select(colnames(order_best_all))
  
  order_best_all <- order_best_all %>% 
    bind_rows(order_best_all_reverse) %>% 
    select(symbol_pair,
           original_symbol_pair,
           baseAsset, 
           quoteAsset,
           askPrice, askQty,
           bidPrice, bidQty,
           minQty_filter,
           minNotation)
  
  # Generate all possible "coin flow" with USDT as start and USDT as end
  # O: USDT;
  # A: Sell USDT to buy A; Sell A to buy B;
  # B: Sell B to buy O; 
  decide_trade_df <- symbol_can_be_traded_in_usd %>%
    left_join(order_best_all %>% select(AB_pair_askPrice = askPrice,
                                        AB_pair_askQty   = askQty,
                                        AB_pair_bidPrice = bidPrice,
                                        AB_pair_bidQty   = bidQty, 
                                        AB_minQty_filter = minQty_filter,
                                        AB_original_pair = original_symbol_pair,
                                        AB_pair_trade    = symbol_pair,
                                        AB_pair_minNotation    = minNotation),
              by = c("pair_AB" = "AB_pair_trade")) %>% 
    filter(!is.na(AB_pair_askPrice)) %>% 
    mutate(AB_trade_direction = ifelse(AB_original_pair != pair_AB, "SELL", "BUY"),
           AB_available_trade_price = ifelse(AB_trade_direction == "SELL", 
                                             AB_pair_bidPrice, 
                                             AB_pair_askPrice),
           AB_available_trade_Qty   = ifelse(AB_trade_direction == "SELL",
                                             AB_pair_bidQty,
                                             AB_pair_askQty)) %>% 
    
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
           AB_Value_in_O = AB_available_trade_Qty*BO_pair_bidPrice,
           BO_Value_in_O = BO_pair_bidQty*BO_pair_bidPrice,
           
           OA_buy_quantity = O_value/OA_pair_askPrice,
           OA_buy_quantity_r = round(OA_buy_quantity/OA_minQty_filter, 0)*OA_minQty_filter,
           
           AB_trade_quatity = ifelse(AB_trade_direction == "SELL", 
                                     OA_buy_quantity_r,
                                     OA_buy_quantity_r/AB_available_trade_price),
           
           AB_trade_quatity_r  = round(AB_trade_quatity/AB_minQty_filter, 0)*AB_minQty_filter,
           AB_trade_quatity_fr = floor(AB_trade_quatity/AB_minQty_filter)*AB_minQty_filter,
           # BO Trade
           BO_trade_quatity = ifelse(AB_trade_direction == "SELL", 
                                     AB_trade_quatity_fr*AB_available_trade_price,
                                     AB_trade_quatity_fr),
           BO_trade_quatity_r  = round(BO_trade_quatity/BO_minQty_filter, 0)*BO_minQty_filter,
           BO_trade_quatity_fr = floor(BO_trade_quatity/BO_minQty_filter)*BO_minQty_filter,
           
           A_left_over     = ifelse(AB_trade_direction == "SELL", 
                                    AB_trade_quatity - OA_buy_quantity_r,
                                    (AB_trade_quatity - AB_trade_quatity_fr)*AB_available_trade_price),
           B_left_over     = BO_trade_quatity - BO_trade_quatity_fr,
           
           A_left_over_r     = ifelse(AB_trade_direction == "SELL", 
                                    AB_trade_quatity - OA_buy_quantity_r,
                                    (AB_trade_quatity - AB_trade_quatity_r)*AB_available_trade_price),
           B_left_over_r     = BO_trade_quatity - BO_trade_quatity_r) %>% 
    mutate(
      # result_O = AB_trade_quatity*(1-trading_fee_rate)^2*BO_pair_bidPrice,
      left_over_value_r_A = A_left_over_r*OA_pair_askPrice,
      left_over_value_r_B = B_left_over_r*BO_pair_bidPrice,
      
      left_over_value     = A_left_over*OA_pair_askPrice + B_left_over*BO_pair_bidPrice,
      result_O            = BO_trade_quatity_fr*BO_pair_bidPrice,
      OA_buy_cost         = OA_buy_quantity_r*OA_pair_askPrice,
      profit_O            = result_O - OA_buy_cost*(1 + trading_fee_rate*3),
      profit_O_percentage = 100*profit_O/(OA_buy_quantity_r*OA_pair_askPrice),
      profit_O_plus_left_over       = profit_O + left_over_value,
      profit_O_percentage_left_over = 100*profit_O_plus_left_over/(OA_buy_quantity_r*OA_pair_askPrice),
      
      result_O_r          = ifelse(AB_trade_direction == "SELL", 
                                   OA_buy_quantity_r*AB_available_trade_price*BO_pair_bidPrice,
                                   OA_buy_quantity_r/AB_available_trade_price*BO_pair_bidPrice),
      profit_O_r          = result_O_r - OA_buy_cost*(1 + trading_fee_rate*3),
      timestamp           = Sys.time()
      )

  decide_trade_df %>% filter(
    profit_O > 0,
    # profit_O_percentage  > 0.001,
                             AB_trade_quatity_fr*AB_pair_askPrice >= AB_pair_minNotation,
                             OA_Value_in_O > 10*O_value,
                             AB_Value_in_O > 5*O_value,
                             BO_Value_in_O > 5*O_value
    # ,AB_trade_direction == "BUY"
    ) %>% 
    mutate(time = Sys.time()) %>% 
    arrange(-profit_O) %>% 
    head(1)
}

# function used to check order status until it got filled
check_submitted_order_status <- function(Order_api_return){
  Continue <- F
  while(!Continue){
    p1 = proc.time()
    Continue <- check_order(symbol = content(Order_api_return)$symbol, 
                               orderId = content(Order_api_return)$orderId)$status == "FILLED"
    p2 = proc.time() - p1
    Sys.sleep(max((0.3 - p2[3]), 0)) #basically sleep for whatever is left of the second)
  }
}

# Trade
trade_main <- function(O_value){
  
  price_qty_df <- decide_trade_or_no(O_value)
  
  # Buy order at market price
  if(nrow(price_qty_df) == 1 ){
    
    # Buy order at market price
    OA_buy_result <- place_order_limit(symbol   = price_qty_df$pair_OA,
                                       side     = "BUY",
                                       quantity = price_qty_df$OA_buy_quantity_r,
                                       price    = price_qty_df$OA_pair_askPrice,
                                       timeInForce = "GTC")
    # content(OA_buy_result)
    
    if(content(OA_buy_result)$status != 'FILLED'){
      check_submitted_order_status(OA_buy_result)
    }
    
    # Buy order at market price
    AB_buy_result <- place_order_limit(symbol   = price_qty_df$AB_original_pair,
                                       side     = price_qty_df$AB_trade_direction,
                                       quantity = price_qty_df$AB_trade_quatity_fr,
                                       price    = price_qty_df$AB_pair_askPrice,
                                       timeInForce = "GTC")
    # content(AB_buy_result)
    check_submitted_order_status(AB_buy_result)
    # Sell order at market price
    BO_sell_result <- place_order_limit(symbol   = price_qty_df$pair_BO,
                                        side     = "SELL",
                                        quantity = price_qty_df$BO_trade_quatity_fr,
                                        price    = price_qty_df$BO_pair_bidPrice,
                                        timeInForce = "GTC")
    
    # content(BO_sell_result)
    check_submitted_order_status(BO_sell_result)
    print(paste("Traded at", Sys.time()))
     browser()
    
    }  else{print(paste("No trigger for Trading at", Sys.time()))}
  
}



while(TRUE){
  p1 = proc.time()
  trade_main(15)
  p2 = proc.time() - p1
  Sys.sleep(max((3 - p2[3]), 0)) #basically sleep for whatever is left of the second
}

