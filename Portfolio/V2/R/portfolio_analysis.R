#install.packages(c('tibble', 
#                   'dplyr', 
#                   'readr'))

modules::import(tibble)
modules::import(dplyr)
modules::import(tidyverse)
modules::import(magrittr)

# (def Transaction
#   [:map
#     [:trade-transaction/creation-id uuid?]
#     [:trade-transaction/id {:optional true} uuid?]
#     [:trade-transaction/type [:enum :buy :sell :dividend :reinvest-divi
#                               :corp-action :fee :tax :exchange-fee :stamp-duty]]
#     [:trade-transaction/date inst?]
#     [:trade-transaction/quantity decimal?]
#     [:trade-transaction/price decimal?]
#     [:trade-transaction/fee-of-transaction-id {:optional true} uuid?]
#     [:trade-transaction/instrument-id uuid?]
#     [:trade-transaction/estimated? boolean?]])
# {:share    "Share"
#   :etf      "ETF"
#   :currency "Currency"
#   :crypto   "Crypto"}
# :buy :sell :dividend :reinvest-divi
# :corp-action :fee :tax :exchange-fee :stamp-duty

instrument_type <- list(Share = "Share", ETF = "ETF", Currency = "Currency", Crypto = "Crypto")

#export("transaction_type")
transaction_type <- list(Buy = "Buy", 
                         Sell = "Sell", 
                         Dividend = "Dividend", 
                         Corp_Action = "Corporate Actoion", 
                         Fee = "Fee",
                         Tax = "Tax",
                         Exchange_Fee = "Exchange Fee", 
                         Stamp_Duty = "Stamp Duty")

export("new_transaction")
new_transaction <- function()  {
  transaction <- tribble(~tx_instrument, 
                        ~tx_instr_type,
                         ~tx_date, 
                        ~tx_type, 
                        ~tx_quantity, 
                        ~tx_price, 
                        ~tx_fees, 
                        ~tx_estimated)
  
  transaction$tx_instrument %<>% as.character
  transaction$tx_instr_type %<>% as.character
  transaction$tx_date %<>% as.Date
  transaction$tx_type %<>% as.character
  transaction$tx_quantity %<>% as.numeric
  transaction$tx_price %<>% as.numeric
  transaction$tx_fees %<>% as.numeric
  transaction$tx_estimated %<>% as.logical
}

tx <- new_transaction()

print(tx)
