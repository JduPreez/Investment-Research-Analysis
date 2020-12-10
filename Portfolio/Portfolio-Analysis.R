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

get_last_share_price <- function(share_prices, symbol, type)  {
 symbol_prices <- share_prices[share_prices[,"ticker"] == symbol,]
 last_date <- max(symbol_prices[,"ref.date"], na.rm = TRUE)
 lsp <- symbol_prices[symbol_prices[,"ref.date"] == last_date,]
 lsp[, paste("price.", type, sep="")]
}

first.date <- Sys.Date() - 10
last.date <- Sys.Date()
freq.data <- "daily"

share_classification <- read_excel(here("Data", "Share_Classification.xlsx"))
trades <- read_excel(here("Data", "TradesExecuted_8077573_2020-01-01_2020-10-09.xlsx"), sheet=2)
closed_positions <- read_excel(here("Data", "ClosedPositions_8077573_2020-01-01_2020-10-09.xlsx"))
colnames(closed_positions)[colnames(closed_positions) == "OpenPostionId"] <- "Trade ID"

open_trades <- trades[trades[,"Open/Close"] == "Open",]
#open_trades <- trades[trades[,"Open/Close"] == "Open" & trades[,"Underlying Instrument Symbol"] == "NTES:xnas",]

open_closed_trades <- merge(x = open_trades, y = closed_positions, by = "Trade ID", all.x = TRUE)

open_positions <- open_closed_trades[is.na(open_closed_trades[,"Trade Date"]),]

open_positions_sorted <- open_positions[order(open_positions[,"Symbol"]),]

open_positions_details <- merge(x = open_positions_sorted, y = share_classification, by = "Symbol")

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

from      <- c("USD", "SEK", "EUR", "GBP", "HKD")
to        <- c("SGD")
fx_rates  <- getQuote(paste0(from, to, "=X"))

View(fx_rates)

last_share_prices <- c()
position_fx_rates <- c()
for (row in 1:nrow(open_positions_details)) {
  last_share_prices <- c(last_share_prices, 
                         get_last_share_price(market_prices[["df.tickers"]],
                                              open_positions_details[row, "Symbol (Yahoo!)"],
                                              "close"))
  
  position_fx_rates <- c(position_fx_rates, 
                         fx_rates[paste(open_positions_details[row, "Currency"], account_currency, "=X", sep=""), "Last"])
}

open_positions_details["last_share_price"] <- last_share_prices
open_positions_details["position_fx_rate"] <- position_fx_rates


# TODO: Convert to account currency
open_positions_details["position_total_close"]  <- open_positions_details[, "Amount.x"] * 
                                                    open_positions_details[, "last_share_price"] * 
                                                    open_positions_details[, "position_fx_rate"]


colnames(open_positions_details)[colnames(open_positions_details) == "Booked Amount"] <- "position_total_open"

holding_total_close <- aggregate(open_positions_details[, "position_total_close"], by=list(open_positions_details[, "Instrument"]), FUN=sum)


View(holding_total_close[order(-holding_total_close[,"x"]),])
