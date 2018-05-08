library(httr)
library(RCurl)   # for base64Decode(), base64Encode(), getURLContent(), and getCurlHandle()
library(digest)  # for hmac()
library(RJSONIO) # for toJSON() and fromJSON()
library(yaml)

config_params <- yaml.load_file(input = "./config.yaml")[["Binance2"]]
recvWindow <- 5000

api.key <- config_params$key
secret <- config_params$secret


# binance api wrapper 
base_url <- "https://api.binance.com/api/"
api_version <- "v1/"



# Check server time
# GET /api/v1/time
# exchange info
get_system_time <- function(){
  api_version <- "v1/"
  end_point <- "time"
  
  url <- paste0(base_url, api_version, end_point) 
  req <- httr::GET(url)
  content <- httr::content(req, "text")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json$serverTime
  
}


# V1 which does not require the users' api key; Public API Endpoints 

# exchange info
get_symbols_list <- function(){
  
  end_point <- "exchangeInfo"
  
  url <- paste0(base_url, api_version, end_point) 
  req <- httr::GET(url)
  content <- httr::content(req, "text")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json$symbols
  
}

# order_book
## order book of a given symbol pair
get_order_book <- function(symbol_pair){
  end_point <- "depth"
  api_version <- "v1/"
  
  url <- paste0(base_url, api_version, end_point) 
  req <- httr::GET(url, query = list(symbol=symbol_pair, limit=5))
  content <- httr::content(req, "text")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json
  
}

## best(highest bid and lowest ask) order book of any pairs
get_orderbook_tickers <- function(){
  api_version <- "v1/"
  
  end_point <- "ticker/allBookTickers"
  
  url <- paste0(base_url, api_version, end_point) 
  req <- httr::GET(url)
  content <- httr::content(req, "text")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json
  
}


# Symbol price ticker
get_symbol_price_ticker <- function(symbol = NULL){
  api_version <- "v3/"
  end_point <- "ticker/bookTicker"
  
  url <- paste0(base_url, api_version, end_point) 
  req <- httr::GET(url, query = list(symbol=symbol))
  content <- httr::content(req, "text")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json
  
}









# Place Order

## New order (TRADE)
# POST /api/v3/order  (HMAC SHA256)






# test post order 
# POST /api/v3/order/test (HMAC SHA256)

test_post_order <- function(){
  
  # ==============================
  url="https://api.binance.com/api/v3/order/test"
  # url="https://api.binance.com/api/v3/order"
  apiKey <- config_params$key
  secretKey <- config_params$secret
  
  timestamp <- round(as.numeric(unclass(Sys.time()))*1000, 0)
  
  symbol = "LTCUSDT"
  side   = "BUY"
  type   = "MARKET"
  quantity  = "0.00001"
  
  postmsg <- paste0("timestamp=", timestamp, "&recvWindow=", recvWindow,
                    "&symbol=", symbol, "&side=",side, 
                    "&type=", type, "&quantity=", quantity)
  signature <- openssl::sha256(postmsg, key=secretKey)
  
  req <- (POST(url, 
               config = add_headers('X-MBX-APIKEY' = binance_key()),
               body=list(timestamp=timestamp,
                         recvWindow=recvWindow, 
                         signature=signature,
                         symbol = symbol, 
                         side   = side,
                         type   = type,
                         quantity  = quantity
                         
               ),
               encode = "form"))
  req
  
  content(req)
  
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json
  
}





url <- 'https://api.binance.com/api/v3/account'
apiKey <- config_params$key
secretKey <- config_params$secret

timestamp <- round(as.numeric(unclass(Sys.time()))*1000, 0)
# timestamp <- 1516941586

postmsg <- paste0("timestamp=", timestamp, "&recvWindow=", recvWindow)
signature <- openssl::sha256(postmsg, key=secretKey)

req <- (GET(url, 
            config = add_headers('X-MBX-APIKEY' = binance_key()),
            query=list(timestamp=timestamp,
                recvWindow=recvWindow, 
               signature=signature)))
req

# ======================

