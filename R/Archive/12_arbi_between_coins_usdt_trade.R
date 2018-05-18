sell_order_example <- x

O_value = 20

trade_main <- function(O_value = 20){
  
  
  price_qty_df <- main_f(O_value)
  
  # price_qty_df$OA_pair_original
  # price_qty_df$OA_buy_quantity
  # price_qty_df$AB_pair_original
  # price_qty_df$AB_Side
  # 
  # 
  # price_qty_df$profit_O
  
  # Buy order at market price
  
  if(nrow(price_qty_df) == 1 ){
    
    if(price_qty_df$AB_Side == "BUY"){
      
      # Buy order at market price
      
      OA_buy_result <- binance_post(endpoint = '/api/v3/order',
                                      params = list(symbol   = price_qty_df$OA_pair_original,
                                                    side     = "BUY",
                                                    type     = "MARKET",
                                                    quantity = as.character(price_qty_df$OA_buy_quantity_r)))
      # content(OA_buy_result)
      
      
      # Buy order at market price
      AB_buy_result <- binance_post(endpoint = '/api/v3/order',
                                    params = list(symbol   = price_qty_df$AB_pair_original,
                                                  side     = price_qty_df$AB_Side,
                                                  type     = "MARKET",
                                                  quantity = as.character(price_qty_df$AB_trade_quatity_r)))
      # content(AB_buy_result)
      
      
      # Sell order at market price
      BO_sell_result <- binance_post(endpoint = '/api/v3/order',
                                     params = list(symbol   = price_qty_df$BO_pair_original,
                                                   side     = "SELL",
                                                   type     = "MARKET",
                                                   quantity = as.character(price_qty_df$BO_trade_quatity_r) ))
      # content(BO_sell_result)
      
      print(paste("Traded at", Sys.time()))
      browser()

    }
  }  else{print(paste("No trigger for Trading at", Sys.time()))}
  
}


while(TRUE){
  p1 = proc.time()
  trade_main(15)
  p2 = proc.time() - p1
  Sys.sleep(max((3 - p2[3]), 0)) #basically sleep for whatever is left of the second
}
