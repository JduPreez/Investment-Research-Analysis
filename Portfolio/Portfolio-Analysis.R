#install.packages(c("tidyverse"))
#install.packages(c("gridExtra", "BatchGetSymbols"))
#install.packages("remotes")
#install.packages("priceR")
#install.packages("data.table")

library(data.table)
library(here)
library(readxl)
library(tibble)
library(tidyverse)
library(gridExtra)
library(grid)
library(BatchGetSymbols)
library(quantmod)
library(priceR)

options(scipen = 999)
options(digits = 6)

account_currency <- "SGD"

get_last_share_price <- function(share_prices, symbol, type, fraction)  {
  # For some reason sometimes duplicate share_prices are returned for a day.
  # So 1st remove these.
  #share_prices <- share_prices %>%
  #                distinct(price.open, price.high, price.low, price.close, price.adjusted, ref.date, ticker)
  
  fraction <- ifelse(!is.na(fraction), fraction, 1)
  symbol_prices <- share_prices[share_prices[,"ticker"] == symbol,]
  last_date <- max(symbol_prices[,"ref.date"], na.rm = TRUE)
  lsp <- symbol_prices[symbol_prices[,"ref.date"] == last_date,]
  (lsp[, paste("price.", type, sep="")]/fraction) %>%
    first()
}

first.date <- Sys.Date() - 10
last.date <- Sys.Date()
freq.data <- "daily"

# TODO!!!!!!!!!!!!!!!!!:  NetEase was rolled up into one position & the separate positions were 
#                         'silently' closed by the broker
#                         NetEase stock was split. How to handle stock splits?

share_classification <- read_excel(here("Data", "Share_Classification.xlsx"))
#share_classification <- share_classification[share_classification[,"Symbol"] == "NTES:xnas",]

trades <- read_excel(here("Data", "TradesExecuted.xlsx"), sheet=2)
closed_positions <- read_excel(here("Data", "ClosedPositions.xlsx"))
colnames(closed_positions)[colnames(closed_positions) == "OpenPostionId"] <- "Trade ID"

trades["Amount"] <- abs(trades[,"Amount"])
open_trades <- trades[trades[,"Open/Close"] == "Open",]

# TODO: Change this to by.x, by.y so that we don't have to rename the column
open_trades_with_closed_pos <- merge(x = open_trades, y = closed_positions, by = "Trade ID", all.x = TRUE)

open_positions <- open_trades_with_closed_pos[is.na(open_trades_with_closed_pos[, "Trade Date Close"]),] %>% data.table()

stock_splits <- merge(x = open_positions, 
                      y = trades[trades[,"B/S"] == "Sold",], 
                      by = c("Symbol", "TradeTime")) %>% 
                filter(Amount.x > Amount.y) %>% 
                select("Symbol", "TradeTime", "Amount.y") %>%
                data.table()

open_positions <- merge(x = open_positions,
                        y = stock_splits,
                        by = "Symbol",
                        all.x = TRUE) %>%
                  filter(is.na(TradeTime.y) | TradeTime.x >= TradeTime.y) %>%
                  data.table() %>%
                  arrange("Symbol")

# open_positions_sorted <- open_positions[order(open_positions[,"Symbol"]),]

open_positions_details <- merge(x = open_positions, y = share_classification, by = "Symbol")

symbols <- unique(share_classification[,"Symbol (Yahoo!)"])[[1]]

first.date <- Sys.Date() - 5
last.date <- Sys.Date()
freq.data <- "daily"

market_prices <- BatchGetSymbols(tickers = symbols,
                                first.date = first.date,
                                last.date = last.date,
                                freq.data = freq.data,
                                cache.folder = file.path('BGS_Cache'))

# View(sapply(open_positions_details, class))
#View(open_positions_details)

first_trade_date = min(open_positions_details[["TradeTime.x"]])
last_trade_date <- max(open_positions_details[["TradeTime.x"]])

to_curs <- c("USD", "SEK", "EUR", "GBP", "HKD", "SGD") # TODO: Get this from the spread sheet

historic_fx_rates <- data.frame(date=as.Date(character()))

for (i in 1:length(to_curs)) {
  if (i == 1) {
    # TODO: Get start & end date from spead sheet
    historic_fx_rates <- historical_exchange_rates(account_currency, to = to_curs[i],
                                                  start_date = "2017-12-25",
                                                  end_date = "2020-12-18")
  }

  if (i > 1) {
    currency_rates <- historical_exchange_rates(account_currency, to = to_curs[i],
                                                start_date = "2017-12-25",
                                                end_date = "2020-12-18")
    historic_fx_rates <- historic_fx_rates %>% left_join(currency_rates, by = "date")
  }
}

# Use a different function for the latest fx rate, to save download size
# in case there's a gap between the last trade date (used as the end_date for the historic
# rates) and today, which we then don't have to download
fx_rates  <- exchange_rate_latest(account_currency)
fx_rates[] <- lapply(fx_rates, function(x) if(is.factor(x)) as.character(x) else x)

str(fx_rates)

last_share_prices <- c()
position_open_fx_rates <- c()
position_close_fx_rates <- c()
position_fx_rates <- c()
for (row in 1:nrow(open_positions_details)) {
  #message(paste("FACTOR", open_positions_details[row, "Factor"], sep=""))
  last_share_price <- get_last_share_price(market_prices[["df.tickers"]],
                                           as.character(open_positions_details[row, "Symbol (Yahoo!)"]),
                                           "close",
                                           as.numeric(open_positions_details[row, Factor]))
  
  last_share_prices <- c(last_share_prices, last_share_price)
  
  share_currency <- open_positions_details[row, Currency]
  trade_time <- open_positions_details[row, TradeTime.x]
  
  position_open_fx_rate <- 1
  position_close_fx_rate <- 1

  if (share_currency != account_currency) {
    position_open_fx_rate <- historic_fx_rates %>%
                              filter(date == trade_time) %>%
                              slice_head() %>%
                              pull(paste("one_", account_currency, "_equivalent_to_x_", share_currency, sep=""))
    
    position_close_fx_rate <- fx_rates %>%
                              filter(currency == share_currency) %>%
                              pull(2)
  }

  position_open_fx_rates <- c(position_open_fx_rates, position_open_fx_rate)

  position_close_fx_rates <- c(position_close_fx_rates, position_close_fx_rate)
}

open_positions_details <- cbind(open_positions_details, last_share_price = last_share_prices)
open_positions_details <- cbind(open_positions_details, position_open_fx_rate = position_open_fx_rates)
open_positions_details <- cbind(open_positions_details, position_close_fx_rate = position_close_fx_rates)

View(open_positions_details)

open_positions_details <- cbind(open_positions_details, position_total_open = (abs(open_positions_details[,"Trade Value"])/
                                                                                  open_positions_details[,"position_open_fx_rate"]) %>%
                                                                                pull("Trade Value"))

open_positions_details <- cbind(open_positions_details, position_total_close = ((open_positions_details[,"Amount"] *
                                                                                   open_positions_details[,"last_share_price"])/
                                                                                  open_positions_details[,"position_close_fx_rate"]) %>%
                                                                                pull("Amount"))

open_holdings_summary <- open_positions_details %>%
                          group_by(Instrument) %>%
                          summarise(holding_total_open = sum(position_total_open),
                                    holding_total_close = sum(position_total_close)) %>%
                          arrange(desc(holding_total_close))

View(open_holdings_summary)