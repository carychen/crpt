credentials <- yaml.load_file(input = "./config.yaml")[["Binance2"]]


## #############################################################################
## Utils

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
  params$timestamp <- timestamp_f()
  params$signature <- hmac(
    key = binance_secret(),
    object = paste(
      mapply(paste, names(params), params, sep = '=', USE.NAMES = FALSE),
      collapse = '&'),
    algo = 'sha256')
  params
}

binance_sign_post <- 
  
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










binance_query <- function(endpoint, method = 'GET',
                          params = list(), sign = FALSE,
                          retry = method == 'GET') {
  
  # method <- match.arg(method)
  
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