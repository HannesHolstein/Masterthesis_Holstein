setwd("C:\\Users\\Holstein\\Documents\\r\\Masterthesis_Holstein_new")
library(tidyverse)
library(xts)
test1 <- read.csv(file ="Mappe1.csv",sep=";")
test <- rev(test1)
test$Close <- c(NA,-diff(test$Close[+1])/test$Close)

test1 %>% map_df(rev)
test2 <- test1 %>% map_df(rev)
test2$Close <- c(-diff(test2$Close )/test2$Close [-1], NA)






######

test3 <- test2 %>% map_df(rev)
library(tidyquant)
library(timetk)

daily_returns <- text
  tq_transmute(select = adjusted,           # this specifies which column to select
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "nflx_returns") # renames the column



library(quantmod)
getSymbols("test", from="2010-08-16",to="2022-01-18")
monthlyReturn(test$Close)

return=(diff(log(test$Close)))
do.call(rbind.data.frame, return)

diff(test$Close)
