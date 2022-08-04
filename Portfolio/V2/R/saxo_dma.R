#library(modules)

# modules::import(tidyquant)
# modules::import(tibble)
# modules::import(tidyverse)
# modules::import(readxl)

dividends <- module({
  import("dplyr")
  
  export("total_dividend_amount")
  total_dividend_amount <- function (aggregated_amounts_tbl) {
    corp_actions <- dplyr::filter(aggregated_amounts_tbl, 
                                  aggregated_amounts_tbl[,"Amount Type Name"] == "Corporate Actions - Cash Dividends" |
                                  aggregated_amounts_tbl[,"Amount Type Name"] == "Corporate Actions - Fractions" |
                                  aggregated_amounts_tbl[,"Amount Type Name"] == "Corporate Actions - Withholding Tax")
    sum(corp_actions[,"Amount Account Currency"])
  }
})

portfolio <- module({
  import("tibble")
  import("dplyr")
  
  export("average_value")
  average_value <- function (aggregated_amounts_tbl) {
    time_period <- aggregated_amounts_tbl %>%
                    dplyr::summarize(start_date = min(Date, na.rm = TRUE),
                                      end_date = max(Date, na.rm = TRUE))
    
    start_rows <- dplyr::filter(aggregated_amounts_tbl, Date == time_period$start_date,
                                aggregated_amounts_tbl[,"Amount Type Name"] == "Position Values" |
                                aggregated_amounts_tbl[,"Amount Type Name"] == "Cash")
    
    end_rows <- dplyr::filter(aggregated_amounts_tbl, Date == time_period$end_date,
                              aggregated_amounts_tbl[,"Amount Type Name"] == "Position Values" |
                              aggregated_amounts_tbl[,"Amount Type Name"] == "Cash")
    
    portfolio_start_value <- sum(start_rows[,"Amount Account Currency"])
    portfolio_end_value <- sum(end_rows[,"Amount Account Currency"])
    portfolio_avg_value <- (portfolio_start_value + portfolio_end_value)/2
    portfolio_avg_value
  }
})
