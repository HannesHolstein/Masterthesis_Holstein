#packages
library(crypto2)
library(dplyr)

########################

#1. Bitcoin: crypto data list
crypto_list()
coins_btc <- crypto_list(only_active=TRUE)
coins_btc
coins_btc$row_num <- seq.int(nrow(coins_btc))

#1.1 Bitcoin: info list
crypto_info(coins_btc,limit=1)
coin_info_btc <- crypto_info(coins_btc,limit=1)
coin_info_btc

#1.2 Bitcoin: price time series data
coin_hist_btc <- crypto_history(coins_btc, limit=1, start_date="20160101", end_date="20211201")
coin_hist_btc_closing <- data.frame(coin_hist_btc$timestamp,coin_hist_btc$close)
# remove hour time stamp



########################

#2. Ethereum: crypto data list
crypto_list()
coins_eth <- crypto_list(only_active=TRUE)
coins_eth
coins_eth$row_num <- seq.int(nrow(coins_eth))
coins_eth_slice <- coins_eth %>% slice(164:164)

#2.2 Ethereum: price time series data
coin_hist_eth <- crypto_history(coins_eth_slice, limit=1, start_date="20160101", end_date="20220101")
coin_hist_eth_closing1 <- data.frame(coin_hist_eth$timestamp,coin_hist_eth$close)
# remove hour time stamp
coin_hist_eth_closing <- coin_hist_eth_closing1
coin_hist_eth_closing$coin_hist_eth.timestamp <- gsub(" 23:59:59","",as.character(coin_hist_eth_closing1$coin_hist_eth.timestamp))
# save as csv
write.table(coin_hist_eth_closing,"C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data\\aggregated\\eth.csv",sep=";", row.names = TRUE)

