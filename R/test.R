setwd("C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data")

library(tidyverse)
library(xts)

test1 <- read.csv(file ="Mappe1.csv",sep=";")
test <- rev(test1)
test1 %>% map_df(rev)
test2 <- test1 %>% map_df(rev)
test2$Close <- c(-diff(log(test2$Close )/test2$Close [-1]), NA)
test3 <- test2 %>% map_df(rev)


test3$date <- as.Date(test3$ï..Date)
data <- test3

data_new1 <- data                                   # Duplicate data
data_new1$year <- strftime(data_new1$date, "%Y")    # Create year column
data_new1$month <- strftime(data_new1$date, "%m")   # Create month column

data_aggr1 <- aggregate(Close ~ month + year,       # Aggregate data
                        data_new1,
                        FUN = sum)

install.packages("lubridate")                       # Install & load lubridate
library("lubridate")

data_new2 <- data                                   # Duplicate data
data_new2$year_month <- floor_date(data_new2$date,  # Create year-month column
                                   "month")

install.packages("dplyr")                           # Install dplyr package
library("dplyr")                                    # Load dplyr

data_aggr2 ＜- data_new2 %＞%                         # Aggregate data
  group_by(year_month) %＞%
  dplyr::summarize(Close =sum(Close)) %＞%
  as.data.frame()




