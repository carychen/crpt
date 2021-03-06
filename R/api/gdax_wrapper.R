# gdax REST API wrappers

library(httr)
library(RCurl)   # for base64Decode(), base64Encode(), getURLContent(), and getCurlHandle()
library(digest)  # for hmac()
library(RJSONIO) # for toJSON() and fromJSON()
library(yaml)
library(dplyr)
# ============================= Basic API functions ============================

#' Look up gdax API secret stored in the environment
#' @return string
#' @keywords internal
gdax_secret <- function() {
  gdax_check_credentials()
  credentials$secret
}

#' Look up gdax API key stored in the environment
#' @return string
#' @keywords internal
gdax_key <- function() {
  gdax_check_credentials()
  credentials$key
}

#' Sets the API key and secret to interact with the gdax API
#' @param key string
#' @param secret string
#' @export
#' @examples \dontrun{
#' gdax_credentials('foo', 'bar')
#' }
gdax_credentials <- function(key, secret) {
  credentials$key <- key
  credentials$secret <- secret
}

#' Check if gdax credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
gdax_check_credentials <- function() {
  if (is.null(credentials$secret)) {
    stop('gdax API secret not set? Call gdax_credentials()')
  }
  if (is.null(credentials$key)) {
    stop('gdax API key not set? Call gdax_credentials()')
  }
}

#' Sign the query string for gdax
#' @param params list
#' @return string
#' @keywords internal
#' @importFrom digest hmac
#' @examples \dontrun{
#' signature(list(foo = 'bar', z = 4))
#' }
gdax_sign <- function(params) {
  params$timestamp <- timestamp_f()
  params$signature <- hmac(
    key = gdax_secret(),
    object = paste(
      mapply(paste, names(params), params, sep = '=', USE.NAMES = FALSE),
      collapse = '&'),
    algo = 'sha256')
  params
}

#' Request the gdax API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required
#' @param retry allow retrying the query on failure
#' @return R object
#' @keywords internal

gdax_query <- function(endpoint, params = list(), sign = FALSE) {
  
  query(base  = 'https://api.gdax.com/',
        path   = endpoint,
        params = params,
        sign   = sign)
  
}

#' POST Request the gdax API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required

gdax_post <- function(endpoint, method = 'POST',
                         params = list(), sign = TRUE) {
  
  post(base = 'https://api.gdax.com',
       path = endpoint,
       method = method,
       params = params)
  
}

# =============================  Other functions  ==========================
# System checking function
# =========================

# Check server time
get_system_time <- function(){
  req <- gdax_query(endpoint = "/time")
  req$serverTime
}

# Check exchange info
get_exchange_info <- function(){
  req <- gdax_query(endpoint = "api/v1/exchangeInfo")
}


# =======================
# Price query functions
# =======================

# order_book with depth of a given symbol pair (symbol_pair can be NULL)
get_orderbook_price_depth <- function(symbol_pair = 'BTCUSDT', limit = 5){
  req <- gdax_query(endpoint = "api/v1/depth", params = list(symbol=symbol_pair, limit=5))
}

# order book for all pairs (tick level)
# Best(highest bid and lowest ask) order book of any pairs
get_best_order_all_pair <- function(){
  
  order_all_list <- gdax_query(endpoint = "api/v1/ticker/allBookTickers")
  
  symbol_pair    <- sapply(order_all_list, '[[', 1) 
  bidPrice       <- sapply(order_all_list, '[[', 2)
  bidQty         <- sapply(order_all_list, '[[', 3)
  askPrice       <- sapply(order_all_list, '[[', 4)
  askQty         <- sapply(order_all_list, '[[', 5)
  order_all_df   <- as.data.frame(cbind(symbol_pair,
                                        bidPrice, bidQty,
                                        askPrice, askQty),
                                  stringsAsFactors = F) %>% 
    mutate_at(vars(c(starts_with("bid"), starts_with("ask"))), funs(as.numeric))
}


# =======================
# Place Order functions
# =======================

## Place New order (TRADE)
### Market order
place_order_market <- function(symbol, side, quantity){
  
  gdax_post(endpoint = '/api/v3/order',
               params = list(symbol   = toupper(symbol),
                             side     = toupper(side),
                             type     = "MARKET",
                             quantity = as.character(quantity)))
}


### Limit order 
# * GTC (Good-Til-Canceled) orders are effective until they are executed or canceled.
# * IOC (Immediate or Cancel) orders fills all or part of an order immediately and cancels the remaining part of the order.
# * FOK (Fill or Kill) orders fills all in its entirety, otherwise, the entire order will be cancelled.
place_order_limit <- function(symbol, side, quantity, price, timeInForce, test = F){
  gdax_post(endpoint = ifelse(test, '/api/v3/order/test', '/api/v3/order'),
               params = list(symbol   = toupper(symbol),
                             side     = toupper(side),
                             type     = "LIMIT",
                             price    = price,
                             quantity = as.character(quantity),
                             timeInForce = toupper(timeInForce)))
  
}

# =======================
# check order status functions
# =======================

check_order <- function(symbol = 'BNBUSDT', 
                        orderId = 31362076, origClientOrderId	 = NULL){
  gdax_query(endpoint = '/api/v3/order', sign = T,
                params = list(symbol   = toupper(symbol),
                              orderId  = as.character(orderId)))
  
}
# =======================
# Cancel order (TRADE)
# =======================

cancel_order <- function(symbol = 'BNBUSDT', 
                         orderId = 31362076, origClientOrderId	 = NULL){
  gdax_post(endpoint = '/api/v3/order', 
               method = 'DELETE',
               params = list(symbol   = toupper(symbol),
                             orderId  = as.character(orderId)),
               sign = T)
}




# User Account info 

# Current open orders (USER_DATA)
get_current_open_orders <- function(){
  
  gdax_query(endpoint = '/api/v3/openOrders',sign = T)
  
}

# All orders (USER_DATA)
get_all_orders <- function(){
  
  gdax_query(endpoint = '/api/v3/allOrders', sign = T)
  
}

# Account information (USER_DATA) including balance
get_account_info <- function(){
  
  gdax_query(endpoint = '/api/v3/account', sign = T)
  
}




# ======================
# Historical market data
# ======================
get_klines <- function(symbol, interval,
                       startTime= NULL, endTime = NULL){
  
  gdax_query(endpoint = '/api/v1/klines',
                params = list(symbol    = symbol,
                              interval  = interval,
                              startTime = startTime,
                              endTime   = endTime))
  
}

