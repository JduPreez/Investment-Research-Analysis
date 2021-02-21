# # Retieve AUD to USD exchange rates
# au <- historical_exchange_rates("AUD", to = "USD",
#                                 start_date = "2020-01-01", end_date = "2020-06-30")
# 
# # Retieve AUD to EUR exchange rates
# ae <- historical_exchange_rates("AUD", to = "EUR",
#                                 start_date = "2020-01-01", end_date = "2020-06-30")
# 
# # Combine
# x <- au %>% left_join(ae, by = "date")
# 
# class(x)

library(priceR)
library(tidyverse)

options(scipen = 100); options(digits = 6)

account_currency <- "SGD"
to_curs <- c("USD", "SEK", "EUR", "GBP", "HKD", "SGD")

match(c("EUR"), to_curs)

# currencies_rates <- data.frame(date=as.Date(character()))
# 
# for (i in 1:length(to_curs)) {
#   if (i == 1) {
#     currencies_rates <- historical_exchange_rates(account_currency, to = to_curs[i],
#                                                   start_date = "2020-01-01",
#                                                   end_date = "2020-12-15")
#   }
#   
#   if (i > 1) {
#     currency_rates <- historical_exchange_rates(account_currency, to = to_curs[i],
#                                                 start_date = "2020-01-01",
#                                                 end_date = "2020-12-15")
#     currencies_rates <- currencies_rates %>% left_join(currency_rates, by = "date")
#   }
# }
# 
# View(currencies_rates)
