library(shiny)
library(BatchGetSymbols)
library(shinydashboard)
library(dplyr)
library(plotly)
library(tidyquant)
library(xts)
library(tibble)
library(dygraphs)
library(DT)
library(shinyWidgets)
library(scales)
library(sparklyr)
library(lubridate)
library(aws.s3)
library(coinmarketcapr)
library(prophet)

# setwd("~")

first.date <- Sys.Date()-2000
sp <- GetSP500Stocks()
ovv <- "OVV"


ovv_list <- BatchGetSymbols(tickers = ovv,
                            first.date = first.date,
                            last.date = Sys.Date(),
                            do.cache=TRUE)

source("api_key/api_coinmarket.R")
source("api_key/api_aws_s3.R")
coinmarketcapr::setup(api_key = api_sourced_key)
all_coins <- get_crypto_listings() %>% mutate(year = year(date_added))
all_coins_select <- all_coins$name
annual_mean_usd_data <- all_coins %>% dplyr::group_by(year) %>% summarise(mean_usd = mean(USD_price),
                                                                          stan_dev = sd(USD_price),
                                                                          lower_1 = mean(USD_price) - sd(USD_price),
                                                                          upper_1 = mean(USD_price) + sd(USD_price)) %>% 
  as.data.frame()