setwd("C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data")

library(tidyverse)
library(xts)

library(foreign)
library(caret)
library(Metrics)
library(car)
library(rmarkdown)
library(devtools)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(finalfit)
library(kableExtra)
library(jtools)
library(stargazer)
library(ggplot2)
library(xts)
library(zoo)
library(lubridate)
library(readxl)

citation("foreign")

#BTC
BTC1 <- read.csv(file ="BTC.csv",sep=";")
BTC1 %>% map_df(rev)
BTC2 <- BTC1 %>% map_df(rev)                                                 #calculate daily return
BTC2$Close <- c((-diff(BTC2$Close )/BTC2$Close [-1]), NA)
BTC3 <- BTC2 %>% map_df(rev)
BTC3$Close <- BTC3$Close+1                                                  #daily log return
BTC3$Close <- log(BTC3$Close)                                                   #aggregate monthly log return
BTC3$date <- as.Date(BTC3$ï..Date)
BTC <- BTC3
data_BTC <- BTC                                   # Duplicate data
data_BTC$year <- strftime(data_BTC$date, "%Y")    # Create year column
data_BTC$month <- strftime(data_BTC$date, "%m")   # Create month column
data_aggr_BTC1 <- aggregate(Close ~ month + year,       # Aggregate data
                        data_BTC,
                        FUN = sum)                                                   # load lubridate
library("lubridate")
data_BTC2 <- BTC                                   # Duplicate data
data_BTC2$year_month <- floor_date(data_BTC2$date,  # Create year-month column
                                   "month")
library("dplyr")                                    # Load dplyr
data_aggr_BTC <- data_BTC2 %＞%                         # Aggregate data
  group_by(year_month) %＞%
  dplyr::summarize(Close =sum(Close)) %＞%
  as.data.frame()



#eth
ETH1 <- read.csv(file ="ETH.csv",sep=";")
ETH1 %>% map_df(rev)
ETH2 <- ETH1 %>% map_df(rev)
#calculate daily return
ETH2$Close <- c((-diff(ETH2$Close )/ETH2$Close [-1]), NA)
ETH3 <- ETH2 %>% map_df(rev)
ETH3$Close <- ETH3$Close+1
#daily log return
ETH3$Close <- log(ETH3$Close)
#aggregate monthly log return
ETH3$date <- as.Date(ETH3$ï..Date)
ETH <- ETH3
data_ETH <- ETH
data_ETH$year <- strftime(data_ETH$date, "%Y")
data_ETH$month <- strftime(data_ETH$date, "%m")
data_aggr_ETH1 <- aggregate(Close ~ month + year,
                            data_ETH,
                            FUN = sum)
data_ETH2 <- ETH
data_ETH2$year_month <- floor_date(data_ETH2$date,"month")
data_aggr_ETH <- data_ETH2 %＞%
  group_by(year_month) %＞%
  dplyr::summarize(Close =sum(Close)) %＞%
  as.data.frame()



#gold with pm fixing (since markatable coins were used with closing price).
Gold1 <- read.csv(file ="lbma_gold_pm_usd_1968-04-01_2022-04-01.csv",sep=";")
#closing = pm price for gold due to simplicity in formulas
Gold1 %>% map_df(rev)
Gold2 <- Gold1 %>% map_df(rev)
#calculate daily return
Gold2$Closing <- c((-diff(Gold2$Closing )/Gold2$Closing [-1]), NA)
Gold3 <- Gold2 %>% map_df(rev)
Gold3$Closing <- Gold3$Closing+1
#daily log return
Gold3$Closing <- log(Gold3$Closing)
#aggregate monthly log return
Gold3$date <- as.Date(Gold3$ï..Date)
Gold <- Gold3
data_Gold <- Gold
data_Gold$year <- strftime(data_Gold$date, "%Y")
data_Gold$month <- strftime(data_Gold$date, "%m")
data_aggr_Gold1 <- aggregate(Closing ~ month + year,
                             data_Gold,
                             FUN = sum)
data_Gold2 <- Gold
data_Gold2$year_month <- floor_date(data_Gold2$date,
                                    "month")
data_aggr_Gold <- data_Gold2 %＞%
  group_by(year_month) %＞%
  dplyr::summarize(Closing =sum(Closing)) %＞%
  as.data.frame()




#REIT
REIT1 <- read.csv(file ="WILLREITIND.csv",sep=";")
REIT1 %>% map_df(rev)
REIT2 <- REIT1 %>% map_df(rev)
#calculate daily return
REIT2$Closing <- c((-diff(REIT2$Closing )/REIT2$Closing [-1]), NA)
REIT3 <- REIT2 %>% map_df(rev)
REIT3$Closing <- REIT3$Closing+1
#daily log return
REIT3$Closing <- log(REIT3$Closing)
#aggregate monthly log return
REIT3$date <- as.Date(REIT3$ï..Date)
REIT <- REIT3
data_REIT <- REIT
data_REIT$year <- strftime(data_REIT$date, "%Y")
data_REIT$month <- strftime(data_REIT$date, "%m")
data_aggr_REIT1 <- aggregate(Closing ~ month + year,
                             data_REIT,
                             FUN = sum)
data_REIT2 <- REIT
data_REIT2$year_month <- floor_date(data_REIT2$date,
                                    "month")
data_aggr_REIT <- data_REIT2 %＞%
  group_by(year_month) %＞%
  dplyr::summarize(Closing =sum(Closing)) %＞%
  as.data.frame()


#SP500
SP1 <- read.csv(file ="SP500.csv",sep=";")
SP1 %>% map_df(rev)
SP2 <- SP1 %>% map_df(rev)
#calculate daily return
SP2$Closing <- c((-diff(SP2$Closing )/SP2$Closing [-1]), NA)
SP3 <- SP2 %>% map_df(rev)
SP3$Closing <- SP3$Closing+1
#daily log return
SP3$Closing <- log(SP3$Closing)
#aggregate monthly log return
SP3$date <- as.Date(SP3$ï..Date)
SP <- SP3
data_SP <- SP
data_SP$year <- strftime(data_SP$date, "%Y")
data_SP$month <- strftime(data_SP$date, "%m")
data_aggr_SP1 <- aggregate(Closing ~ month + year,
                           data_SP,
                           FUN = sum)
data_SP2 <- SP
data_SP2$year_month <- floor_date(data_SP2$date,
                                  "month")
data_aggr_SP <- data_SP2 %＞%
  group_by(year_month) %＞%
  dplyr::summarize(Closing =sum(Closing)) %＞%
  as.data.frame()



#Treasury bill
#Cochrane and Piazzesi -> log yield
Six_M_Bill <- read.csv(file="6m_bill.csv", sep=";")
Six_M_Bill$Month <- mdy(Six_M_Bill$Month)






#Slice data for BTC sample size 2014-2021 -> series1/sample1 for BTC

series1_BTC <- data_aggr_BTC %>% slice(40:135)
sample1_BTC <- do.call(cbind.data.frame, series1_BTC)

series1_Gold <- data_aggr_Gold %>% slice(550:645)
sample1_Gold <- do.call(cbind.data.frame, series1_Gold)

series1_REIT <- data_aggr_REIT %>% slice(1:96)
sample1_REIT <-do.call(cbind.data.frame, series1_REIT)

series1_SP <- data_aggr_SP %>% slice(1:96)
sample1_SP <-do.call(cbind.data.frame, series1_SP)

#slice data for ETH sample size to be added







############## regression
CPI <- read.csv(file ="CPI.csv",sep=";")
CPI$Month <- mdy(CPI$Month)


##Regressions for different CPI's used
#Define variable names for regression table output
CPI_all <- CPI$CPIAUCSL_PC1
CPI_less_food_energy<- CPI$CPILFESL_PC1
BTC_logreturn1<-sample1_BTC$Close
Gold_logreturn1<-sample1_Gold$Closing
REIT_logreturn1<-sample1_REIT$Closing
########Treasury_bill_6m_sample<-Six_M_Bill$Moving.1.Month.Average not included yet
SP500_logreturn1<-sample1_SP$Closing

#OLS-regression CPI all consumer
m1_all <- lm(CPI_all~BTC_logreturn1+Gold_logreturn1+REIT_logreturn1+ SP500_logreturn1)

#CPI less food & energy
m2_less <- lm(CPI_less_food_energy~BTC_logreturn1+Gold_logreturn1+REIT_logreturn1+SP500_logreturn1)

# Checking model statistics
tab_model(m1_all)
tab_model(m2_less)

summary(m1_all)
summary(m2_less)
#html table output for regression
stargazer(m1_all,m2_less, type = "html",
          title = "Regression output CPI all urban consumers", out = "m1_reg.html")

##  AIC = – 2 * ln(likelihood) + 2 * p
##  BIC = – 2 * ln(likelihood) + ln(N) * p
## p = number of estimated parameters and N = sample size.
##  AIC and BIC values can be used for choosing the best predictor subsets in regression and for comparing different models.
##  When comparing different models, the model with minimum AIC and BIC values is considered the best model.

# Using AIC function
AIC(m1_all)
AIC(m2_less)

# Using BIC function
BIC(m1_all)
BIC(m2_less)

# RMSE
names(m1_all)
rmse(m1_all)
names(m2_less)
rmse(m2_less)

#histogram of residuals
hist(m1_all$residuals, color = "grey")
hist(m2_less$residuals, color = "grey")

# Using plot function for NPP plot
par(mfrow=c(2,2))
plot(m1_all,main="M1: CPI all consumers")

par(mfrow=c(2,2))
plot(m2_less,main="M2: CPI less food/energy")

#VIF calculation
vif(m1_all)
vif(m1_less)

#autocorrelation Durbin-Watson test
library(lmtest)
citation("lmtest")
dwtest(m1_all)
dwtest(m2_less)

#correlations



