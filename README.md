# crpt

This repository is a R POC tool for cryptocurrency trading strategies. 

If you want to support this repository development, please sign up cryptocurrency exchange using the below reference link: 

1. Binance: https://www.binance.com/?ref=28937757

Language: **R**



### How to use this repo

Download R and Rstudio. Fork and clone the repo to your local. 

* Setup packrat using command: 

packrat::restore()

(Install packrat package if needed.)

* Setup exchange APIs. 

Add a new file "config.yaml" in crpt folder, with you API key and secret in the below format: 

Binance:
  key : 'XXXXXXXXXX'
  secret : 'XXXXXXX'


* Test API:
get_exchange_info() can be used to test public api.
get_account_info() cab be used to test private api. 

* Test sample strategy:

### Folder structure
-- crpt (root folder)
include config.yaml file

---- R
All trading strategy R scripts

------ api
All api util functions and wrapper functions

------ back-testing 

---- packrat
R package version control .lock file

---- data
* test results and other data
* Historical market data

### Exchanges

We have API wrappers in exchanges below:

**Binance**: API Doc [link]

**GDAX**: API Doc [link]

### Strategies 

* triangle-arbitrage
* market maker arbitrage 
