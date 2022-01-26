#packages needed:
library(usethis)
library(fredr)

#API-Key for FRED (user dependent)
usethis::edit_r_environ()

#Search terms for time series
search_gold <- fredr_series_search_text("Gold Fixing Price")
colnames(search_gold)

search_CPI <- fredr_series_search_text("Consumer Price Index: Total")
colnames(search_CPI)

search_REIT <- fredr_series_search_text("Wilshire US Real Estate Investment Trust")
colnames(search_REIT)

search_fixed_income <- fredr_series_search_text("6-Month Treasury Bill")
colnames(search_fixed_income)

search_sp500 <- fredr_series_search_text("stock index")
colnames(search_sp500)



# loading ggplot2 R-package
library(ggplot2)

# DAUPSA is id for seasonlly adjusted monthly domestic car production in units
series_ls <-fredr_series_observations(series_id = "DAUPSA")

# convert series list to dataframe
series_df <- do.call(cbind.data.frame, series_ls)

# plotting data
ggplot(series_df) + geom_line(mapping = aes(x=date,y=value),
                              color = "red") +
  ggtitle("Monthly US car production, seasonally adjusted [in thousands]") +
  xlab("time") +
  ylab("monthly cars produced [thousands of units]")




# B149RC1Q027SBEA is id for US domestic sales of imported new cars, seasonally adjusted and in billions of USD
series_df <-do.call(cbind.data.frame,
                    fredr_series_observations(series_id = "B149RC1Q027SBEA"))

# plotting data
ggplot(series_df) + geom_line(mapping = aes(x=date,y=value),
                              color = "red") +
  ggtitle("Quarterly US imported new car sales, seasonally adjusted [in billion USD]") +
  xlab("time") +
  ylab("quarterly new imported car sales [billion USD]")

