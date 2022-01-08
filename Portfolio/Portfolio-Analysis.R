#install.packages(c("tidyverse"))
#install.packages(c("gridExtra", "BatchGetSymbols"))
#install.packages("remotes")
#install.packages("priceR")
#install.packages("data.table")
#install.packages("ggplot2")

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
library(ggplot2)

####################################################################
# TODO: 
#   - Convert data frames & tables to tibble?????????
#   - Add pie chart for ETF vs individual stocks %
####################################################################

options(scipen = 999)
options(digits = 6)

account_currency <- "SGD"

get_last_share_price <- function(share_prices, symbol, type, fraction)  {
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

share_classification <- read_excel(here("Data", "Share_Classification.xlsx"))
#share_classification <- share_classification[share_classification[,"Symbol"] == "CLNK_B:xome",]

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

open_positions_details <- merge(x = open_positions, y = share_classification, by = "Symbol")

symbols <- unique(share_classification[,"Symbol (Yahoo!)"])[[1]]

first.date <- Sys.Date() - 15 # Need to make it go back 15 days, because BatchGetSymbols breaks if there's a gap in the data
last.date <- Sys.Date() + 1
freq.data <- "daily"

market_prices <- BatchGetSymbols(tickers = symbols,
                                first.date = first.date,
                                last.date = last.date,
                                freq.data = freq.data,
                                cache.folder = file.path('BGS_Cache'))

first_trade_date <- min(open_positions_details[["TradeTime.x"]])
last_trade_date <- max(open_positions_details[["TradeTime.x"]])

to_curs <- share_classification %>%
            distinct(.data[["Currency"]]) %>%
            pull(.data[["Currency"]])

#to_curs <- c("USD", "SEK", "EUR", "GBP", "HKD", "SGD", "JPY") # TODO: Get this from the spread sheet

historic_fx_rates <- data.frame(date=as.Date(character()))

for (i in 1:length(to_curs)) {
  if (i == 1) {
    historic_fx_rates <- historical_exchange_rates(account_currency, to = to_curs[i],
                                                  start_date = first_trade_date,
                                                  end_date = last_trade_date)
  }

  if (i > 1) {
    currency_rates <- historical_exchange_rates(account_currency, to = to_curs[i],
                                                start_date = first_trade_date,
                                                end_date = last_trade_date)
    historic_fx_rates <- historic_fx_rates %>% left_join(currency_rates, by = "date")
  }
}

# Use a different function for the latest fx rate, to save download size
# in case there's a gap between the last trade date (used as the end_date for the historic
# rates) and today, which we then don't have to download
fx_rates  <- exchange_rate_latest(account_currency)
fx_rates[] <- lapply(fx_rates, function(x) if(is.factor(x)) as.character(x) else x)

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

open_positions_details <- cbind(open_positions_details, position_total_open = (abs(open_positions_details[,"Trade Value"])/
                                                                                  open_positions_details[,"position_open_fx_rate"]) %>%
                                                                                pull("Trade Value"))

open_positions_details <- cbind(open_positions_details, position_total_close = ((open_positions_details[,"Amount"] *
                                                                                   open_positions_details[,"last_share_price"])/
                                                                                  open_positions_details[,"position_close_fx_rate"]) %>%
                                                                                pull("Amount"))

all_holdings_total_open <- sum(open_positions_details[,"position_total_open"])
all_holdings_total_close <- sum(open_positions_details[,"position_total_close"])

# holding_open_weight = round((position_total_open/all_holdings_total_open) * 100, 2),
# holding_close_weight = round((position_total_close/all_holdings_total_close) * 100, 2)
# holding_open_weight = round((holding_total_open/all_holdings_total_open) * 100, 2)

open_holdings_summary <- open_positions_details %>%
                          group_by(Instrument,Symbol) %>%
                          summarize(holding_total_open = sum(position_total_open),
                                    holding_total_close = sum(position_total_close)) %>%
                          mutate(holding_open_weight = round((holding_total_open/all_holdings_total_open) * 100, 2)) %>%
                          arrange(desc(holding_open_weight)) %>%
                          inner_join(share_classification, by = "Symbol")

get_chart_data <- function(open_holdings_summary_in, group_by_field) {
                    open_holdings_summary_in %>%
                    group_by(.data[[group_by_field]]) %>%
                    summarize(holding_total_open = sum(holding_total_open), 
                              holding_total_close = sum(holding_total_close)) %>%
                    mutate(holding_total_open_perc = round((holding_total_open/sum(holding_total_open)) * 100, 2),
                           holding_total_close_perc = round((holding_total_close/sum(holding_total_close)) * 100, 2)) # %>%
                    #arrange(desc(holding_total_close_perc)) 
                  }

holdings_by_strategy <- get_chart_data(open_holdings_summary, "Strategy")

holdings_by_asset_type <- get_chart_data(open_holdings_summary, "Asset Type")

data_model         <- list(open_holdings_summary, get_chart_data, holdings_by_strategy, holdings_by_asset_type)
names(data_model)  <- c("open_holdings_summary", "get_chart_data", "holdings_by_strategy", "holdings_by_asset_type")

save(data_model, file = "Portfolio/Portfolio-Analysis.rda")


