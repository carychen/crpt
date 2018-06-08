
# download market spot price data

while(TRUE){
  p1 = proc.time()
  df <- get_spot_price() %>% 
    mutate(date_time = Sys.time())
  
  data.table::fwrite(as.data.frame(df),  append = T,
                     file = paste0("./data/market data/spot.csv"))
  
  p2 = proc.time() - p1
  Sys.sleep(max((1 - p2[3]), 0)) #basically sleep for whatever is left of the second
}


