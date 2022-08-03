# install.packages(c('tibble', 
#                     'dplyr', 
#                     'readr',
#                     'modules'))

library("modules")

portfolio_analysis <- module({
  import("tibble")
  import("dplyr")
  import("tidyverse")
  import("magrittr")
  
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
    transaction
  }
})


#tx <- portfolio_analysis$new_transaction()

#print(tx)
