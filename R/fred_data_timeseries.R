#packages needed:
library(usethis)
library(fredr)
library(readr)

#API-Key for FRED (user dependent)
usethis::edit_r_environ()

#gold data available over csv file in regression - source is noted in thesis

#cpi data
search_CPI <- fredr_series_search_text("Consumer Price Index: Total")
colnames(search_CPI)
series_CPI_all <-fredr_series_observations(series_id = "CPALTT01USM659N")
series_CPI <- series_CPI_all %>% slice(721:792)
series_CPI_df <- do.call(cbind.data.frame, series_CPI)

#reit data
search_reit <- fredr_series_search_text("Wilshire US Real Estate Investment Trust")
colnames(search_reit)
series_reit_all <-fredr_series_observations(series_id = "WILLREITIND")
series_reit <- series_reit_all %>% slice(9916:11481)
series_reit_df <- do.call(cbind.data.frame, series_reit)

#US treasury 6M bill
search_fixed_income <- fredr_series_search_text("6-Month Treasury Bill")
colnames(search_fixed_income)
series_treasury_all <-fredr_series_observations(series_id = "TB6MS")
series_treasury <- series_treasury_all %>% slice(686:757)
series_treasury_df <- do.call(cbind.data.frame, series_treasury)


#S&P500
search_sp500 <- fredr_series_search_text("stock index")
colnames(search_sp500)
series_sp500_all <-fredr_series_observations(series_id = "TB6MS")
series_sp500 <- series_sp500_all %>% slice(686:757)
series_sp500_df <- do.call(cbind.data.frame, series_sp500)








