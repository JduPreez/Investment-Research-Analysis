library(modules)
library(readxl)
library(here)
library(tibble)
library(tidyverse)

options(scipen = 999)
options(digits = 6)

sgdzar <- 11.42

lib <- modules::use(here("Portfolio", "V2", "R"))

aggregated_amounts <- read_excel(here("Portfolio", "V2", "data", "aggregated_amounts.xlsx"))

time_period <- aggregated_amounts %>%
                summarize(start_date = min(Date, na.rm = TRUE),
                end_date = max(Date, na.rm = TRUE))

start_rows <- filter(aggregated_amounts, Date == time_period$start_date,
                                          aggregated_amounts[,"Amount Type Name"] == "Position Values" |
                                          aggregated_amounts[,"Amount Type Name"] == "Cash")

end_rows <- filter(aggregated_amounts, Date == time_period$end_date,
                                        aggregated_amounts[,"Amount Type Name"] == "Position Values" |
                                        aggregated_amounts[,"Amount Type Name"] == "Cash")

portfolio_start_value <- sum(start_rows[,"Amount Account Currency"])
portfolio_end_value <- sum(end_rows[,"Amount Account Currency"])
portfolio_avg_value <- (portfolio_start_value + portfolio_end_value)/2
portfolio_avg_value_zar <- portfolio_avg_value * sgdzar

# corp_actions <- filter(aggregated_amounts, aggregated_amounts[,"Amount Type Name"] == "Corporate Actions - Cash Dividends" |
#                                             aggregated_amounts[,"Amount Type Name"] == "Corporate Actions - Fractions" |
#                                             aggregated_amounts[,"Amount Type Name"] == "Corporate Actions - Withholding Tax")
# 
# saxo_dma_total_divi <- sum(corp_actions[,"Amount Account Currency"])

saxo_dma_total_divi <- lib$saxo_dma$dividends$total_dividend_amount(aggregated_amounts)

# Do it manually - EasyEquities doesn't have an Excel/CSV export of this
# Dividends & interest
ee_divi <- c(1147, 1489, 1021, 349, 39, 20, 7, 13, 4, 25, 31, 63, 35)
ee_total_divi <- sum(ee_divi)


# TODO: Get latest exchange rate from data feed
total_divi_zar <- (saxo_dma_total_divi * sgdzar) + ee_total_divi

portfolio_yield <- (total_divi_zar/portfolio_avg_value_zar) * 100








