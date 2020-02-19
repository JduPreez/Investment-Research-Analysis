# Follow some steps in this article: https://www.datacamp.com/community/tutorials/r-tutorial-read-excel-into-r

#install.packages(c("tidyverse"))
library(readxl)
library(tibble)
library(tidyverse)

company <- "Pfizer"
row_name_column <- "Name"

set_row_names <- function(data_frame, row_name_colmn)  {
   data_frame %>% remove_rownames %>% column_to_rownames(var=row_name_colmn)
}

# balance_sheet_file <- paste("Stocks/Fundamentals/", company, "/Balance-Sheet-Annual.xls", sep="")
income_statement_file <- paste("Stocks/Fundamentals/", company,"/Income-Statement-Annual.xls", sep="")
cash_flow_file <- paste("Stocks/Fundamentals/", company, "/Cash-Flow-Annual.xls", sep="")

# balance_sheet_src <- read_excel(balance_sheet_file)
income_statement_src <- read_excel(income_statement_file)
cash_flow_src <- read_excel(cash_flow_file)

income_statement <- set_row_names(income_statement_src, row_name_column) # income_statement_src %>% remove_rownames %>% column_to_rownames(var="Name")

income_statement[] <- lapply(income_statement, function(val) {
  val <- ifelse(is.na(val), "0", val)
  val <- ifelse(val == "-", "0", val)
  gsub(",", "", val)
})

income_statement["Total Revenue", ]

revenue <- as.numeric(income_statement["Total Revenue", ])

# revenue

diff(revenue)/revenue[-length(revenue)] * 100