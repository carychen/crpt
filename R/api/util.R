library(futile.logger)

#' Return current UNIX timestamp in millisecond
#' @return milliseconds since Jan 1, 1970
#' @keywords internal
timestamp_f <- function() {
  # as.character(round(as.numeric(Sys.time()) * 1e3))
  get_system_time()
}


#' Request the Binance API query(GET)
#' @param base URL
#' @param path string
#' @param method HTTP request method
#' @param params list
#' @param config httr::config
#' @param retry allow retrying the query on failure
#' @param retries internal counter of previous retries
#' @return R object
#' @keywords internal
#' @importFrom httr GET content config add_headers
#' @importFrom futile.logger flog.error
query <- function(base, path, sign = F, 
                  params = list()) {
  url <- paste0(base, path)
  if(sign == T){
    params$timestamp <- timestamp_f()
    params$signature <- openssl::sha256(key = binance_secret(),
                                        x = paste(mapply(paste, names(params), 
                                                         params, sep = '=', USE.NAMES = FALSE),
                                                  collapse = '&'))  
  } 
  if(sign){
    req <- (GET(url, 
                config = add_headers('X-MBX-APIKEY' = binance_key()), 
                query= params,
                encode = "form"))
  } else {
    
    req <- (GET(url, 
                query= params,
                encode = "form")) 
  }
  
  content(req)
  
}

#' Request the Binance API (POST)
#' @param base URL
#' @param path string
#' @param method HTTP request method (POST)
#' @param params list
#' @return R object

post <- function(base, path, method = 'POST',
                 params = list()){
  url <- paste0(base, path)
  params$timestamp <- timestamp_f()
  params$signature <- openssl::sha256(key = binance_secret(),
                                      x = paste(mapply(paste, names(params), 
                                                       params, sep = '=', USE.NAMES = FALSE),
                                                collapse = '&'))
  if(method == 'POST'){
    req <- (POST(url, 
                 config = add_headers('X-MBX-APIKEY' = binance_key()),
                 body= params,
                 encode = "form"))
  } else if(method == 'DELETE'){
    req <- (DELETE(url, 
                 config = add_headers('X-MBX-APIKEY' = binance_key()),
                 body= params,
                 encode = "form"))
    
  }
  
  req
  
}

