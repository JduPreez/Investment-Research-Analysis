#install.packages(c("TTR"))
#library(TTR)

# Rescaled Range:
# https://everything.explained.today/Rescaled_range/
# Implementation in Python: https://github.com/Mottl/hurst

dir = "C:\\Users\\jacqu\\Documents\\Proj\\Investment-Research-Analysis\\Stocks\\"
sharePrices = read.csv(file = paste(dir, "Electronic-Arts-Historical-Prices.csv", sep=""))

#nrow(sharePrices)
#tail(sharePrices)

# Get a vector of the Close column values
# https://stackoverflow.com/questions/7070173/convert-data-frame-column-to-a-vector
closePrices <- sharePrices[["Close"]]

# 1. Calculate the mean
m <- mean(closePrices)

# 2. Create a mean adjusted series
# --------------------------------
#   Subtract m from each value of the series to produce a mean adjusted series
mean_adj_ser <- function(values, mean_val)  {
  mean_adj_ser <- c() # An empty vector   

  for (i in 1:length(values)) {
    mean_adj_ser[i] <- round(values[i] - mean_val, 6)
  }
  mean_adj_ser
}

mas <- mean_adj_ser(closePrices, m)
mas

# 3. Calculate the cumulative deviate series Z
# --------------------------------------------
# Take the first value X, then sum of the first two values X+Y, then sum of the first 
# three values X+Y+Z and so on to create a new series.
cds <- cumsum(mas) # runSD(mas, n=1, cumulative=TRUE)
cds