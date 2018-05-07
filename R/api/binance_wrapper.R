library(httr)
library(RCurl)   # for base64Decode(), base64Encode(), getURLContent(), and getCurlHandle()
library(digest)  # for hmac()
library(RJSONIO) # for toJSON() and fromJSON()

# binance api wrapper 
base_url <- "https://api.binance.com/api/"
api_version <- "v1/"

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

# https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#new-order--trade
dfToJson <- function(df,token,uri){
  ### DF is of the structure
  ### Column 1: Offender GUID
  ### Column 2: Table to update
  ### Column 3: Name of column
  ### Column 4: Value
  ### Column 5: Dbind (TRUE or FALSE)
  ### Column 6: Dbind prefix
  
  ## Get offender GUID from 
  
  #offender_guid <-retrieve_offender_id_info(token,uri,as.character(df[1,1]))
  
  dataToUpdate <- list()
  dataToUpdate[["offender_guid"]] <- as.character(df[1,1])
  dataToUpdate[["table"]] <- as.character(df[1,2])
  
  for(i in 1:nrow(df)){
    
    if(df[i,5] == TRUE){
      
      dataToUpdate[["body_list"]] <- c(dataToUpdate[["body_list"]],o360_pushobject_dbind(as.character(df[i,3]),as.character(df[i,6]),as.character(df[i,4])))
      
    } else{
      
      dataToUpdate[["body_list"]] <- c(dataToUpdate[["body_list"]],o360_pushobject_basic(as.character(df[i,3]),as.character(df[i,4])))
      
    }
    
  }
  
  dataToUpdate[["body_json"]] <- rjson::toJSON(dataToUpdate[["body_list"]])
  
  return(dataToUpdate)
  
}

o360_post <- function(accessToken,
                      organizationURI,
                      entityName,
                      post_content){
  
  header <- o360_pushheaders(accessToken)
  
  base_config <- "api/data/v8.1/%s"
  config <- sprintf(base_config,entityName)
  
  
  queryUrl <- modify_url(organizationURI,path=config)
  
  resp <- httr::POST(url = queryUrl,
                     header,  
                     body = post_content,
                     encode="json"
  )
  
  return(resp)
}





api.key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

order <- list(
  symbol = "BTCUSDT",
  side   = "BUY",
  type   = "MARKET",
  quantity  = "0.1",
  timestamp = as.numeric(Sys.time()) * 1000
)

key <- base64Decode(secret, mode="raw")
body <- toJSON(order, collapse="")
what <- paste0(timestamp, toupper(method), req.url, body)

sign <- base64Encode(hmac(key, what, algo="sha256", raw=TRUE))




# test post order 
# POST /api/v3/order/test (HMAC SHA256)

test_post_order <- function(){
  api_version <- "v3/"
  end_point   <- "order/test"

  url <- paste0(base_url, api_version, end_point) 
  req <- httr::POST(url, body = post_body,
                    encode = "form",
                    verbose())
  
  content <- httr::content(req, "text")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  json
  
}








