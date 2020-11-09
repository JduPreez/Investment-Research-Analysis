# # Compute a mean adjusted series
# # --------------------------------
# # Subtract m from each value of the series to produce a mean adjusted series
# mean_adj_ser <- function(values, mean_val)  {
#   mean_adj_ser <- c() # An empty vector   

#   for (i in 1:length(values)) {
#     mean_adj_ser[i] <- round(values[i] - mean_val, 6)
#   }
#   mean_adj_ser
# }

# # Compute the Rescaled Range
# #-------------------------------------
# # values: numeric vector
# # https://blog.quantinsti.com/hurst-exponent/
# rescaled_range <- function(values) {
#   # Divide values into matrices

# }

# TODO: Test with unequal number of observations
close_prices  <- c(0.04, 0.02, 0.05, 0.08, 0.02, -0.17, 0.05, 0.00, 0.04, 0.02, 0.05, 0.08, 0.02, -0.17, 0.05, 0.00) # c(0.04, 0.02, 0.05, 0.08, 0.02, -0.17, 0.05, 0.00)
observations  <- length(close_prices)
divisions     <- log2(observations)-1 # Subtract 1 to make for loop zero indexed

for (d in 0:divisions) {
  chunks <- 2^d
  observations_per_chunk <- observations/chunks
  x <- c(chunks, observations_per_chunk)
  print(x)
}

# cols <- 1 # i1: 2^0 = 1
          # i2: 2^1 = 2
          # i3: 2^2 = 4
          # i4: 2^3 = 8   -> loop until = number of initial observations
          # i5: 2^4 = 16
          # i6: 2^5 = 32  -> 2 to the power of X? gives you 32?
          #               -> the number of chunks/iterations = log base-2 (no of observations)

# rescaled_range(closePrices)

#division_chunks <- matrix(data = close_prices, 
#                          nrow = , ncol = ncol, byrow = byrow) 
