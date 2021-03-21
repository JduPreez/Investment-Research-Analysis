# Updated: 2021-01-21
#--------------------

# Follow some steps in this article: https://www.datacamp.com/community/tutorials/r-tutorial-read-excel-into-r

#install.packages(c("tidyverse", "gridExtra"))

library(here)
library(readxl)
library(tibble)
library(tidyverse)
library(gridExtra)
library(grid)

options(scipen = 999)

company               <- "Lufax"
row_name_column       <- "LU_IncomeStatement_Annual_As_Originally_Reported"
required_ret_int_rate <- 10
current_share_price   <- 105 # CNY

# Mappings
# Income statement mappings
sales_revenue_row           <- "Total Revenue"
post_tax_ebita_row          <- "Net Income Available to Common Stockholders"

# Cash flow statement mappings
net_operating_cash_flow_row <- "Cash Flow from Operating Activities, Indirect"
capital_expenditures_row    <- "Purchase/Sale and Disposal of Property, Plant and Equipment, Net"

# Balance sheet mappings
debt_row                    <- c("Debt and Capital Lease Obligations")
shares_outstanding_row      <- "Common Shares Outstanding"
cash_short_term_invest_row  <- "Cash and Cash Equivalents" # "Cash, Cash Equivalents and Short Term Investments"

set_row_names <- function(data_frame)  {
  row_name_column <- colnames(data_frame)[1]
  data_frame %>% remove_rownames %>% column_to_rownames(var=row_name_column)
}

ensure_numbers <- function(val) {
  val <- ifelse(is.na(val), "0", val)
  val <- ifelse(val == "-", "0", val)
  val <- gsub(",", "", val)
  as.numeric(val)
}

# TODO: Don't stop here, rather add each row not found to a collection
#       and if this list has 1 or more items in stop and print all missing rows.
ensure_rows_exist_recur <- function(rows_search, data_set) {
  if (length(rows_search) > 1) {
    for (row_search in rows_search) {
      ensure_rows_exist_recur(row_search, data_set)
    }
  }

  if (length(rows_search) == 1) {
    is_contained <- rows_search %in% row.names(data_set)
    if (!(is_contained)) {
      stop(paste("Row not found: ", rows_search), call. = FALSE)
    }
  }
}

balance_sheet_file      <- paste("Stocks/Fundamentals/", company, "/", sep="")
income_statement_file   <- paste("Stocks/Fundamentals/", company,"/", sep="")
cash_flow_file          <- paste("Stocks/Fundamentals/", company, "/", sep="")

income_statement_src    <- read_excel(here("Data", "Companies", company, "Income-Statement-Annual.xls"))
cash_flow_src           <- read_excel(here("Data", "Companies", company, "Cash-Flow-Annual.xls"))
balance_sheet_src       <- read_excel(here("Data", "Companies", company, "Balance-Sheet-Annual.xlsx"))

income_statement        <- set_row_names(income_statement_src)
cash_flow               <- set_row_names(cash_flow_src)
balance_sheet           <- set_row_names(balance_sheet_src)

# Check that each data set has the required rows
ensure_rows_exist_recur(list(sales_revenue_row, post_tax_ebita_row), income_statement)
ensure_rows_exist_recur(list(net_operating_cash_flow_row, capital_expenditures_row), cash_flow)
ensure_rows_exist_recur(list(debt_row, shares_outstanding_row, cash_short_term_invest_row), balance_sheet)

income_statement[]      <- lapply(income_statement, ensure_numbers)
income_statement[]      <- lapply(income_statement, as.numeric)
income_statement$TTM    <- NULL
cash_flow[]             <- lapply(cash_flow, ensure_numbers)
cash_flow$TTM           <- NULL
balance_sheet[]         <- lapply(balance_sheet, ensure_numbers)

sales_revenue           <- as.numeric(income_statement[sales_revenue_row, ])
post_tax_ebita          <- as.numeric(income_statement[post_tax_ebita_row, ])

net_operating_cash_flow <- as.numeric(cash_flow[net_operating_cash_flow_row, ])
capital_expenditures    <- as.numeric(cash_flow[capital_expenditures_row, ])

shares_outstanding      <- as.numeric(balance_sheet[shares_outstanding_row, ])
cash_short_term_invest  <- as.numeric(balance_sheet[cash_short_term_invest_row, ])

sales_growth_perc <- diff(sales_revenue)/sales_revenue[-length(sales_revenue)] * 100 # Sales-Revenue Growth %

# Makes oldest year's sales growth 0
# to ensure same number of years/columns as other data points 
sales_growth_perc <- c(0, sales_growth_perc)

# Free Cash Flow Margin %
# Net Operating Cash Flow - Capital Expenditures/Sales-Revenue
free_cash_flow_margin_perc <- (net_operating_cash_flow - capital_expenditures)/sales_revenue * 100

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

# Cash Flow to Debt
# Cash and short-term investments/Total Debt
# https://www.investopedia.com/terms/c/cash-flowtodebt-ratio.asp
cash_flow_to_debt <- (net_operating_cash_flow/net_debt) * 100

net_operating_cash_flow
net_debt

# Create matrix for grid table
analysis            <- rbind(sales_growth_perc,                            
                            free_cash_flow_margin_perc, 
                            epv_perc_of_share,
                            cash_flow_to_debt)

# TODO: Add Return On Capital Employed: https://simplywall.st/news/we-like-these-underlying-trends-at-netdragon-websoft-holdings-hkg777/
rownames(analysis)  <- c("Sales-Revenue Growth %",                           
                          "Free Cash Flow Margin %",
                          "EPV as % of share price",
                          "Cash Flow to Debt")

colnames(analysis)  <- colnames(balance_sheet) # Get years from balance sheet colnames


dev.new(width=100, height=5)
grid.table(analysis)

