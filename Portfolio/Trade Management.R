#install.packages(c("tidyverse"))
#install.packages(c("gridExtra", "BatchGetSymbols"))
#install.packages("remotes")

library(here)
library(readxl)
library(tibble)
library(tidyverse)
library(gridExtra)
library(grid)
library(BatchGetSymbols)

first.date <- Sys.Date() - 10
last.date <- Sys.Date()
freq.data <- "daily"

share_classification <- read_excel(here("Data", "Share_Classification.xlsx"))
trades <- read_excel(here("Data", "TradesExecuted_8077573_2020-01-01_2020-10-09.xlsx"), sheet=2)
closed_positions <- read_excel(here("Data", "ClosedPositions_8077573_2020-01-01_2020-10-09.xlsx"))
colnames(closed_positions)[colnames(closed_positions) == "OpenPostionId"] <- "Trade ID"

open_trades <- trades[trades[,"Open/Close"] == "Open",]
#open_trades <- trades[trades[,"Open/Close"] == "Open" & trades[,"Underlying Instrument Symbol"] == "NTES:xnas",]

open_closed_trades <- merge(x = open_trades, y = closed_positions, by = "Trade ID", all.x = TRUE)

open_positions <- open_closed_trades[is.na(open_closed_trades[,"Trade Date"]),]

open_positions_sorted <- open_positions[order(open_positions[,"Symbol"]),]

open_positions_details <- merge(x = open_positions_sorted, y = share_classification, by = "Symbol")

symbols <- unique(share_classification[,"Symbol (Yahoo!)"])[[1]]

first.date <- Sys.Date() - 5
last.date <- Sys.Date()
freq.data <- "daily"

symbols

l.out <- BatchGetSymbols(tickers = symbols, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache'))

View(l.out)