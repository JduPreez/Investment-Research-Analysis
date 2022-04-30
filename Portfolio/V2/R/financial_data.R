modules::import(tidyverse)
modules::import(tidyquant)

# x <- tq_get(c("ULVR.L", "MO"), get = "dividends", from = "2019-01-01")

# get_dividend <- function (symbol, from_dte) {
#   tq_get(symbol, get = "dividends", from = from_dte)
# }
# 

export("get_dividends")
get_dividends <- function (symbols, from_dte) {
  tq_get(symbols, get = "dividends", from = from_dte)
}