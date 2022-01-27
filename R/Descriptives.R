
library(ggplot2)
library(psych)

# 1.1 Building plots BTC
##density
ggplot(data=coin_hist_btc, aes(coin_hist_btc$close)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

##line plot
plot(y=coin_hist_btc$close, x=coin_hist_btc$timestamp, col="red", type = "l",
     xlab = "Time",
     ylab = "Price",
     main = "Bitcoin Price in USD",
     sub = "End of Day Closing Prices from Coinmarketcap")

# 1.2 Building plots ETH
##density
ggplot(data=coin_hist_eth, aes(coin_hist_eth$close)) +
  geom_histogram(aes(y =..density..), fill = "yellow") +
  geom_density()

##line plot
plot(y=coin_hist_eth$close, x=coin_hist_eth$timestamp, col="orange", type = "l",
     xlab = "Time",
     ylab = "Price",
     main = "Ethereum Price in USD",
     sub = "End of Day Closing Prices from Coinmarketcap")

# 1.3 Building plots Gold Price
tba

# 1.4 Building plots REIT
tba

# 1.5 Building plots US 6M Bill
tba

# 1.6 Building plots S&P500
tba


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
