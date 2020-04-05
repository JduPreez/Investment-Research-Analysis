# Follow some steps in this article: https://www.datacamp.com/community/tutorials/r-tutorial-read-excel-into-r

# install.packages(c("tidyverse", "gridExtra"))
library(readxl)
library(tibble)
library(tidyverse)
library(gridExtra)
library(grid)

options(scipen = 999)

company               <- "Pfizer"
row_name_column       <- "Name"
required_ret_int_rate <- 10
current_share_price   <- 11.99

# Mappings
sales_revenue_row           <- "Total Revenue"
net_operating_cash_flow_row <- "Cash Flow from Operating Activities, Indirect"
capital_expenditures_row    <- "Purchase/Sale and Disposal of Property, Plant and Equipment, Net"
post_tax_ebita_row          <- "Net Income Available to Common Stockholders"
debt_row                    <- c("Current Portion of Long Term Debt", "Long Term Debt", "Current Debt")
shares_outstanding_row      <- "Common Shares Outstanding"

set_row_names <- function(data_frame, row_name_colmn)  {
   data_frame %>% remove_rownames %>% column_to_rownames(var=row_name_colmn)
}

ensure_numbers <- function(val) {
  val <- ifelse(is.na(val), "0", val)
  val <- ifelse(val == "-", "0", val)
  val <- gsub(",", "", val)
  as.numeric(val)
}

balance_sheet_file      <- paste("Stocks/Fundamentals/", company, "/Balance-Sheet-Annual.xls", sep="")
income_statement_file   <- paste("Stocks/Fundamentals/", company,"/Income-Statement-Annual.xls", sep="")
cash_flow_file          <- paste("Stocks/Fundamentals/", company, "/Cash-Flow-Annual.xls", sep="")

income_statement_src    <- read_excel(income_statement_file)
income_statement        <- set_row_names(income_statement_src, row_name_column)
income_statement[]      <- lapply(income_statement, ensure_numbers)
income_statement[]      <- lapply(income_statement, as.numeric)
income_statement$TTM    <- NULL

cash_flow_src           <- read_excel(cash_flow_file)
cash_flow               <- set_row_names(cash_flow_src, row_name_column)
cash_flow[]             <- lapply(cash_flow, ensure_numbers)
cash_flow$TTM           <- NULL

balance_sheet_src       <- read_excel(balance_sheet_file)
balance_sheet           <- set_row_names(balance_sheet_src, row_name_column)
balance_sheet[]         <- lapply(balance_sheet, ensure_numbers)

sales_revenue           <- as.numeric(income_statement[sales_revenue_row, ])
net_operating_cash_flow <- as.numeric(cash_flow[net_operating_cash_flow_row, ])
capital_expenditures    <- as.numeric(cash_flow[capital_expenditures_row, ])
post_tax_ebita          <- as.numeric(income_statement[post_tax_ebita_row, ])
shares_outstanding      <- as.numeric(balance_sheet[shares_outstanding_row, ])

# Sales-Revenue Growth %
diff(sales_revenue)/sales_revenue[-length(sales_revenue)] * 100

# Free Cash Flow Margin %
# Net Operating Cash Flow - Capital Expenditures/Sales-Revenue
(net_operating_cash_flow - capital_expenditures)/sales_revenue * 100

# Implied Earnings Power Value (EPV)
# Post Tax EBITA/(Required Rate/100)
implied_epv   <- post_tax_ebita/(required_ret_int_rate/100)

# Equity Value
# Implied EPV - Net Debt
net_debt      <- as.numeric(colSums(balance_sheet[debt_row,]))
equity_value  <- implied_epv - net_debt

# EPV per share
# Equity Value/Shares Outstanding
epv_per_share <- equity_value/shares_outstanding

# EPV as % of share price
epv_perc_of_share <- (epv_per_share/current_share_price) * 100
epv_perc_of_share

# TODO: Use grid.table to put data into a summary table:
# https://stackoverflow.com/questions/32926718/r-gridextra-how-to-plot-a-summary-as-table