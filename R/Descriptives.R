library(ggplot2)

# 1.1 Building plots BTC
ggplot(data=coin_hist_btc, aes(coin_hist_btc$close)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

plot(y=coin_hist_btc$close, x=coin_hist_btc$timestamp, col="red", type = "l",
     xlab = "Time",
     ylab = "Price",
     main = "Bitcoin Price in USD",
     sub = "End of Day Closing Prices from Coinmarketcap")

# 1.2 Building plots ETH
ggplot(data=coin_hist_eth, aes(coin_hist_eth$close)) +
  geom_histogram(aes(y =..density..), fill = "yellow") +
  geom_density()

plot(y=coin_hist_eth$close, x=coin_hist_eth$timestamp, col="orange", type = "l",
     xlab = "Time",
     ylab = "Price",
     main = "Ethereum Price in USD",
     sub = "End of Day Closing Prices from Coinmarketcap")

# 1.3 Building histogram Gold Price
tba

# 1.4 Building histogram REIT
tba

# 1.5 Building histogram US 6M Bill
tba

# 1.6 Building histogram S&P500
tba




# loading psych package
library(psych)
psych::describe(housing)


library(reshape)
meltData <- melt(housing)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")


require(corrgram)
corrgram(housing, order=TRUE)
