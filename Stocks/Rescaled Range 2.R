# Compute a mean adjusted series
# --------------------------------
# Subtract m from each value of the series to produce a mean adjusted series
mean_adj_ser <- function(values, mean_val)  {
  mean_adj_ser <- c() # An empty vector   

  for (i in 1:length(values)) {
    mean_adj_ser[i] <- round(values[i] - mean_val, 6)
  }
  mean_adj_ser
}

# Compute the Rescaled Range
#-------------------------------------
# values: numeric vector
# https://blog.quantinsti.com/hurst-exponent/
rescaled_range <- function(values) {
  # Divide values into matrices

}

closePrices <- c(0.04, 0.02, 0.05, 0.08, 0.02, -0.17, 0.05, 0.00)
rescaled_range(closePrices)
