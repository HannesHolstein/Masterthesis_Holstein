
library(ggplot2)
library(psych)


#2. psych-package descriptive statistics
btc_descriptives <- psych::describe(coin_hist_btc$close)
eth_descriptives <- psych::describe(coin_hist_eth$close)
gold_desriptives <- psych::describe() #tba
reit_descriptives <- psych::describe() #tba
us_6m_bill_descriptives <- psych::describe() #tba
sp500_descriptives <- psych::describe() #tba
cpi_descriptives <- psych::describe() #tba

#3. Outliers (to be added)
boxplot(coin_hist_btc$close)


# correlation matrix (to be added)
require(corrgram)
corrgram(coin_hist, order=TRUE)
