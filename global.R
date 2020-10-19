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

# setwd("~")

first.date <- Sys.Date()-2000
sp <- GetSP500Stocks()
ovv <- "OVV"
sp_list <- BatchGetSymbols(tickers = sp$Tickers,
                           first.date = first.date,
                           last.date = Sys.Date(),
                           do.cache=TRUE)

ovv_list <- BatchGetSymbols(tickers = ovv,
                            first.date = first.date,
                            last.date = Sys.Date(),
                            do.cache=TRUE)