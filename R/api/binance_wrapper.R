# Binance REST API wrappers

library(httr)
library(RCurl)   # for base64Decode(), base64Encode(), getURLContent(), and getCurlHandle()
library(digest)  # for hmac()
library(RJSONIO) # for toJSON() and fromJSON()
library(yaml)
library(dplyr)
# ============================= Basic API functions ============================

#' Look up Binance API secret stored in the environment
#' @return string
#' @keywords internal
binance_secret <- function() {
  binance_check_credentials()
  credentials$secret
}

#' Look up Binance API key stored in the environment
#' @return string
#' @keywords internal
binance_key <- function() {
  binance_check_credentials()
  credentials$key
}

#' Sets the API key and secret to interact with the Binance API
#' @param key string
#' @param secret string
#' @export
#' @examples \dontrun{
#' binance_credentials('foo', 'bar')
#' }
binance_credentials <- function(key, secret) {
  credentials$key <- key
  credentials$secret <- secret
}

#' Check if Binance credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
binance_check_credentials <- function() {
  if (is.null(credentials$secret)) {
    stop('Binance API secret not set? Call binance_credentials()')
  }
  if (is.null(credentials$key)) {
    stop('Binance API key not set? Call binance_credentials()')
  }
}

#' Sign the query string for Binance
#' @param params list
#' @return string
#' @keywords internal
#' @importFrom digest hmac
#' @examples \dontrun{
#' signature(list(foo = 'bar', z = 4))
#' }
binance_sign <- function(params) {
  params$timestamp <- timestamp()
  params$signature <- hmac(
    key = binance_secret(),
    object = paste(
      mapply(paste, names(params), params, sep = '=', USE.NAMES = FALSE),
      collapse = '&'),
    algo = 'sha256')
  params
}

#' Request the Binance API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required
#' @param retry allow retrying the query on failure
#' @return R object
#' @keywords internal
binance_query <- function(endpoint, method = 'GET',
                          params = list(), sign = FALSE,
                          retry = method == 'GET') {

  method <- match.arg(method)
  
  if (isTRUE(sign)) {
    params <- binance_sign(params)
    config <- add_headers('X-MBX-APIKEY' = binance_key())
  } else {
    config <- config()
  }
  
  query(
    base = 'https://api.binance.com',
    path = endpoint,
    method = method,
    params = params,
    config = config)
  
}

#' POST Request the Binance API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required

binance_post <- function(endpoint, method = 'POST',
                         params = list(), sign = TRUE) {
  
  post(base = 'https://api.binance.com',
       path = endpoint,
       method = method,
       params = params)
  
}

# =============================  Other functions  ==========================
# System checking function
# =========================

# Check server time
get_system_time <- function(){
  req <- binance_query(endpoint = "api/v1/time")
  req$serverTime
}

# Check exchange info
get_exchange_info <- function(){
  req <- binance_query(endpoint = "api/v1/exchangeInfo")
}


# =======================
# Price query functions
# =======================

# order_book with depth of a given symbol pair (symbol_pair can be NULL)
get_orderbook_price_depth <- function(symbol_pair = 'BTCUSDT', limit = 5){
  req <- binance_query(endpoint = "api/v1/depth", params = list(symbol=symbol_pair, limit=5))
}

# order book for all pairs (tick level)
# Best(highest bid and lowest ask) order book of any pairs
get_best_order_all_pair <- function(){
  
  order_all_list <- binance_query(endpoint = "api/v1/ticker/allBookTickers")
  
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
  
  binance_post(endpoint = '/api/v3/order',
               params = list(symbol   = toupper(symbol),
                             side     = toupper(side),
                             type     = "MARKET",
                             quantity = as.character(quantity)))
}


### Limit order 
# * GTC (Good-Til-Canceled) orders are effective until they are executed or canceled.
# * IOC (Immediate or Cancel) orders fills all or part of an order immediately and cancels the remaining part of the order.
# * FOK (Fill or Kill) orders fills all in its entirety, otherwise, the entire order will be cancelled.
place_order_limit <- function(symbol, side, quantity, price, timeInForce){
  binance_post(endpoint = '/api/v3/order',
               params = list(symbol   = toupper(symbol),
                             side     = toupper(side),
                             type     = "LIMIT",
                             price    = price,
                             quantity = as.character(quantity),
                             timeInForce = toupper(timeInForce)))
  
}


