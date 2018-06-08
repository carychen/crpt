# load library and source files
source("./R/api/util.R")
source("./R/api/binance_wrapper.R")

# Get account balance for all coins
get_account_balance <- function(){
  account_balance_list <- get_account_info()$balance
  
  asset           <- sapply(account_balance_list, '[[', 1)
  free_balance    <- sapply(account_balance_list, '[[', 2)
  locked_balance  <- sapply(account_balance_list, '[[', 3)
  
  account_balance_df <- as.data.frame(cbind(asset, free_balance, locked_balance),
                                      stringsAsFactors = F) %>% 
    mutate(free_balance = as.numeric(free_balance),
           locked_balance = as.numeric(locked_balance),
           total_balance = free_balance+locked_balance)
}

balance_checker_main <- function(){
  
  account_balance_raw_df <- get_account_balance() %>% 
    mutate(date_time = Sys.time())
}

while(TRUE){
  p1 = proc.time()
  balance_check_df <- balance_checker_main()
  
  data.table::fwrite(as.data.frame(balance_check_df),  append = T,
                     file = paste0("./data/view/balance_checkter.csv"))

  p2 = proc.time() - p1
  Sys.sleep(max((4 - p2[3]), 0)) #basically sleep for whatever is left of the second
}







