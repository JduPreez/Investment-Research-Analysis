#install.packages(c("tidyverse"))
#install.packages(c("gridExtra", "BatchGetSymbols"))
#install.packages("remotes")

library(here)
library(readxl)
library(tibble)
library(tidyverse)
library(gridExtra)
library(grid)
library(BatchGetSymbols)
library(quantmod)

options(scipen = 999)

account_currency <- "SGD"

get_last_share_price <- function(share_prices, symbol, type, fraction)  {
  fraction <- ifelse(!is.na(fraction), fraction, 1)
  symbol_prices <- share_prices[share_prices[,"ticker"] == symbol,]
  last_date <- max(symbol_prices[,"ref.date"], na.rm = TRUE)
  lsp <- symbol_prices[symbol_prices[,"ref.date"] == last_date,]
  lsp[, paste("price.", type, sep="")]/fraction
}

first.date <- Sys.Date() - 10
last.date <- Sys.Date()
freq.data <- "daily"

share_classification <- read_excel(here("Data", "Share_Classification.xlsx"))
trades <- read_excel(here("Data", "TradesExecuted.xlsx"), sheet=2)
closed_positions <- read_excel(here("Data", "ClosedPositions.xlsx"))
colnames(closed_positions)[colnames(closed_positions) == "OpenPostionId"] <- "Trade ID"

open_trades <- trades[trades[,"Open/Close"] == "Open",]

open_closed_trades <- merge(x = open_trades, y = closed_positions, by = "Trade ID", all.x = TRUE)

open_positions <- open_closed_trades[is.na(open_closed_trades[,"Trade Date"]),]

open_positions_sorted <- open_positions[order(open_positions[,"Symbol"]),]

open_positions_details <- merge(x = open_positions_sorted, y = share_classification, by = "Symbol")

View(open_positions_details)

symbols <- unique(share_classification[,"Symbol (Yahoo!)"])[[1]]

first.date <- Sys.Date() - 5
last.date <- Sys.Date()
freq.data <- "daily"

market_prices <- BatchGetSymbols(tickers = symbols, 
                                first.date = first.date,
                                last.date = last.date, 
                                freq.data = freq.data,
                                cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache'))

from      <- c("USD", "SEK", "EUR", "GBP", "HKD", "SGD")
to        <- c(account_currency)

# TODO!!!!!:
# Switch to using trade value + historical exchange rates
# https://github.com/stevecondylios/priceR

fx_rates  <- getQuote(paste0(from, to, "=X"))

last_share_prices <- c()
position_fx_rates <- c()
for (row in 1:nrow(open_positions_details)) {
  #message(paste("FACTOR", open_positions_details[row, "Factor"], sep=""))
  last_share_prices <- c(last_share_prices, 
                         get_last_share_price(market_prices[["df.tickers"]],
                                              open_positions_details[row, "Symbol (Yahoo!)"],
                                              "close",
                                              open_positions_details[row, "Factor"]))
  
  share_currency <- open_positions_details[row, "Currency"]
  position_fx_rates <- ifelse(share_currency == account_currency, 1,
                              c(position_fx_rates, 
                                fx_rates[paste(share_currency, account_currency, "=X", sep=""), "Last"]))
}

open_positions_details["last_share_price"] <- last_share_prices
open_positions_details["position_fx_rate"] <- position_fx_rates

open_positions_details["position_total_close"]  <- open_positions_details[, "Amount.x"] * 
                                                    open_positions_details[, "last_share_price"] * 
                                                    open_positions_details[, "position_fx_rate"]


colnames(open_positions_details)[colnames(open_positions_details) == "Booked Amount"] <- "position_total_open"

open_holdings_summary <- aggregate(list(holding_total_open = open_positions_details[, "position_total_open"]), 
                                   by=list(instrument = open_positions_details[, "Instrument"]), FUN=sum)

holding_total_close <- aggregate(list(holding_total_close = open_positions_details[, "position_total_close"]), 
                                 by=list(instrument = open_positions_details[, "Instrument"]), FUN=sum)

open_holdings_summary["holding_total_close"] <- holding_total_close[,"holding_total_close"]

View(open_holdings_summary[order(-open_holdings_summary[,"holding_total_close"]),])
