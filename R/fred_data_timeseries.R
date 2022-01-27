#packages needed:
library(usethis)
library(fredr)
library(readr)

#API-Key for FRED (user dependent)
usethis::edit_r_environ()

#gold data

search_gold <- fredr_series_search_text("Gold price")
colnames(search_gold)
series_gold_all <- read_csv("C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Masterthesis_Holstein_R\\GOLDAMGBD228NLBM.csv")
series_gold <- series_gold_all %>% slice(12460:14025)
series_gold_df <- do.call(cbind.data.frame, series_gold)

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




# plotting data
ggplot(series_df) + geom_line(mapping = aes(x=date,y=value),
                              color = "red") +
  ggtitle("Monthly US car production, seasonally adjusted [in thousands]") +
  xlab("time") +
  ylab("monthly cars produced [thousands of units]")



