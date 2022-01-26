install.packages("crypto2", dependencies = TRUE)
library(crypto2)
library(dplyr)

#1. Bitcoin: crypto data list
crypto_list()
coins_btc <- crypto_list(only_active=TRUE)
coins_btc
coins_btc$row_num <- seq.int(nrow(coins_btc))

#1.1 Bitcoin info list
crypto_info(coins_btc,limit=1)
coin_info_btc <- crypto_info(coins_Btc,limit=1)
coin_info_btc

#1.2 Bitcoin time series
coin_hist_btc <- crypto_history(coins_btc, limit=1, start_date="20160101", end_date="20210105")

#2. Ethereum: crypto data list
crypto_list()
coins_eth <- crypto_list(only_active=TRUE)
coins_eth
coins_eth$row_num <- seq.int(nrow(coins_eth))
coins_eth_slice <- coins_eth %>% slice(164:164)

coin_hist_eth <- crypto_history(coins_eth_slice, limit=1, start_date="20160101", end_date="20210105")

coin_hist_eth_closing <- data.frame(coin_hist_eth$timestamp,coin_hist_eth$close)












coin_hist %>% group_by(slug) %>% slice(1:2)

fiats <- fiat_list()
fiats

coin_hist2 <- crypto_history(coins, convert="BTC,EUR", limit=3, start_date="20210101", end_date="20210105")

coin_hist2 %>% group_by(slug,ref_cur) %>% slice(1:2)

exchanges <- exchange_list(only_active=TRUE)
exchanges

ex_info <- exchange_info(exchanges %>% filter(slug %in% c('binance','kraken')))
ex_info


ex_info %>% select(contains("fee"))
ex_info %>% select(contains("spot"))



ex_info %>% select(slug,fiats) %>% tidyr::unnest(fiats)
