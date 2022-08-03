library("modules")

# modules::import(tidyverse)
# modules::import(tidyquant)
modules::import("readxl")
# 
# # x <- tq_get(c("ULVR.L", "MO"), get = "dividends", from = "2019-01-01")
# 
# # get_dividend <- function (symbol, from_dte) {
# #   tq_get(symbol, get = "dividends", from = from_dte)
# # }
# # 
# 
# export("get_dividends")
# get_dividends <- function (symbols, from_dte) {
#   tq_get(symbols, get = "dividends", from = from_dte)
# }

dividends <- module({
  #import("readxl")
  
  export("total_dividend_amount")
  total_dividend_amount <- function (aggregated_amounts_tbl) {
    corp_actions <- filter(aggregated_amounts_tbl, 
                            aggregated_amounts_tbl[,"Amount Type Name"] == "Corporate Actions - Cash Dividends" |
                            aggregated_amounts_tbl[,"Amount Type Name"] == "Corporate Actions - Fractions" |
                            aggregated_amounts_tbl[,"Amount Type Name"] == "Corporate Actions - Withholding Tax")
    sum(corp_actions[,"Amount Account Currency"])
  }
})