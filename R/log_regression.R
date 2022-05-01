setwd("C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data")

library(tidyverse)
library(xts)
library(MASS)
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
library(dplyr)

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

data_BTC2 <- BTC                                   # Duplicate data
data_BTC2$year_month <- floor_date(data_BTC2$date,  # Create year-month column
                                   "month")
                                   # Load dplyr
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
Bill1 <- read.csv(file="6m_bill.csv", sep=";")
Bill1$Closing <- as.numeric(Bill1$Closing) +1
Bill1 %>% map_df(rev)
Bill2 <- Bill1 %>% map_df(rev)
Bill3 <- Bill2 %>% map_df(rev)
#daily log return
Bill3$Closing <- log(Bill3$Closing)
#aggregate monthly log return
Bill3$ï..Date <- as.Date(Bill3$ï..Date)
Bill <- Bill3
data_Bill <- Bill
data_Bill$year <- strftime(data_Bill$ï..Date, "%Y")
data_Bill$month <- strftime(data_Bill$ï..Date, "%m")
data_aggr_Bill1 <- aggregate(Closing ~ month + year,
                             data_Bill,
                             FUN = sum,na.rm = TRUE)
data_Bill2 <- Bill
data_Bill2$year_month <- floor_date(data_Bill2$ï..Date,
                                    "month")
data_aggr_Bill <- data_Bill2 %＞%
  group_by(year_month) %＞%
  dplyr::summarize(Closing =sum(Closing,na.rm = TRUE)) %＞%
  as.data.frame()




#Slice data for BTC sample size 2014-2021 -> series1/sample1 for BTC

series1_BTC <- data_aggr_BTC %>% slice(40:135)
sample1_BTC <- do.call(cbind.data.frame, series1_BTC)

series1_Gold <- data_aggr_Gold %>% slice(550:645)
sample1_Gold <- do.call(cbind.data.frame, series1_Gold)

series1_REIT <- data_aggr_REIT %>% slice(1:96)
sample1_REIT <-do.call(cbind.data.frame, series1_REIT)

series1_SP <- data_aggr_SP %>% slice(1:96)
sample1_SP <-do.call(cbind.data.frame, series1_SP)

series1_Bill <- data_aggr_Bill %>% slice(662:757)
sample1_Bill <- do.call(cbind.data.frame, series1_Bill)

#descriptive statistics for log returns
mean_btc<-mean(sample1_BTC$Close)
mean_eth<-mean(sample2_eth$Close)
mean_gold<-mean(sample1_Gold$Closing)
mean_reit<-mean(sample1_REIT$Closing,na.rm = TRUE)
mean_sp<-mean(sample1_SP$Closing,na.rm = TRUE)
mean_bill<-mean(sample1_Bill$Closing)

min_btc<-min(sample1_BTC$Close)
min_eth<-min(sample2_eth$Close)
min_gold<-min(sample1_Gold$Closing)
min_reit<-min(sample1_REIT$Closing,na.rm = TRUE)
min_sp<-min(sample1_SP$Closing,na.rm = TRUE)
min_bill<-min(sample1_Bill$Closing)

max_btc<-max(sample1_BTC$Close)
max_eth<-max(sample2_eth$Close)
max_gold<-max(sample1_Gold$Closing)
max_reit<-max(sample1_REIT$Closing,na.rm = TRUE)
max_sp<-max(sample1_SP$Closing,na.rm = TRUE)
max_bill<-max(sample1_Bill$Closing)

sd_btc<-sd(sample1_BTC$Close)
sd_eth<-sd(sample2_eth$Close)
sd_gold<-sd(sample1_Gold$Closing)
sd_reit<-sd(sample1_REIT$Closing,na.rm = TRUE)
sd_sp<-sd(sample1_SP$Closing,na.rm = TRUE)
sd_bill<-sd(sample1_Bill$Closing)


df1 = data.frame(Variable = c('BTC','ETH','REIT','Gold','SP','Bill'),
                 n=c(96,96,96,96,96,96),
                 mean=c(mean_btc,mean_eth,mean_reit,mean_gold,mean_sp,mean_bill),
                 min=c(min_btc,min_eth,min_reit,min_gold,min_sp,min_bill),
                 max=c(max_btc,max_eth,max_reit,max_gold,max_sp,max_bill),
                 sd=c(sd_btc,sd_eth,sd_reit,sd_gold,sd_sp,sd_bill))

write.table(df1,file="C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data\\descriptives.txt")


#correlation matrix
series2_BTC <- data_aggr_BTC %>% slice(64:135)
sample2_BTC <- do.call(cbind.data.frame, series2_BTC)
df2 = data.frame(Bitcoin = sample2_BTC$Close, ETH =sample2_eth$Close, REIT= sample2_REIT$Closing,
                 Gold = sample2_Gold$Closing, SP =sample2_SP$Closing, Bill = sample2_Bill$Closing)
cor(df2)

#BTC plot
ggplot(sample1_BTC, mapping=aes(x=year_month,y=Close,group=1))+
  geom_line() +
  ggtitle("Bitcoin") +
  scale_y_continuous(name="aggr. monthly log return",breaks = c(0,0.1,0.25,0.5,0.75,1),limits=c(-1,1))
#Gold plot
ggplot(sample1_Gold, mapping=aes(x=year_month,y=Closing,group=1))+
  geom_line() +
  ggtitle("Gold") +
  scale_y_continuous(name="aggr. monthly log return",breaks = c(0,0.1,0.25,0.5,0.75,1),limits=c(-1,1))
#ETH plot
ggplot(series1_ETH, mapping=aes(x=year_month,y=Close,group=1))+
  geom_line() +
  ggtitle("ETH") +
  scale_y_continuous(name="aggr. monthly log return",breaks = c(0,0.1,0.25,0.5,0.75,1),limits=c(-1,1))
#REIT
ggplot(sample1_REIT, mapping=aes(x=year_month,y=Closing,group=1))+
  geom_line() +
  ggtitle("Wilshire REIT index") +
  scale_y_continuous(name="aggr. monthly log return",breaks = c(0,0.1,0.25,0.5,0.75,1),limits=c(-1,1))
#SP500 plot
ggplot(sample1_SP, mapping=aes(x=year_month,y=Closing,group=1))+
  geom_line() +
  ggtitle("S&P500") +
  scale_y_continuous(name="aggr. monthly log return",breaks = c(0,0.1,0.25,0.5,0.75,1),limits=c(-1,1))

#6m Bill plot
ggplot(sample1_Bill, mapping=aes(x=year_month,y=Closing,group=1))+
  geom_line() +
  ggtitle("6-Month treasury bill") +
  scale_y_continuous(name="aggr. monthly log return",breaks = c(0,0.1,0.25,0.5,0.75,1),limits=c(-1,1))



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
bill_loginterest1<-sample1_Bill$Closing
SP500_logreturn1 <- sample1_SP$Closing



#OLS-regression CPI all consumer
m1_all <- lm(CPI_all~BTC_logreturn1+Gold_logreturn1+REIT_logreturn1+ SP500_logreturn1+bill_loginterest)

#CPI less food & energy
m2_less <- lm(CPI_less_food_energy~BTC_logreturn1+Gold_logreturn1+REIT_logreturn1+SP500_logreturn1+bill_loginterest)


l1 <- lm(CPI_all~BTC_logreturn1)
l2 <- lm(CPI_all~Gold_logreturn1)
l3 <- lm(CPI_all~REIT_logreturn1)
l4 <- lm(CPI_all~SP500_logreturn1)
l5 <- lm(CPI_all~bill_loginterest1)
stargazer(l1,l2,l3,l4,l5, type = "html",
          title = "test", out = "test.html")



# Checking model statistics
tab_model(m1_all)
tab_model(m2_less)

summary(m1_all)
summary(m2_less)
#html table output for regression
stargazer(m1_all,m2_less, type = "html",
          title = "Regression output CPI all urban consumers", out = "btc_reg.html")

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

RSS_m1 <- c(crossprod(m1_all$residuals))
RSS_m2 <- c(crossprod(m2_less$residuals))

#Mean squared error:
MSE_m1 <- RSS_m1 / length(m1_all$residuals)
MSE_m2 <- RSS_m2 / length(m2_less$residuals)

#Root MSE:
RMSE_m1 <- sqrt(MSE_m1)
RMSE_m2 <- sqrt(MSE_m2)

#Pearson estimated residual variance (as returned by summary.lm):
sig2_m1 <- RSS_m1 / m1_all$df.residual
sig2_m2 <- RSS_m2 / m2_less$df.residual
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
vif(m2_less)

#autocorrelation Durbin-Watson test
library(lmtest)
citation("lmtest")
dwtest(m1_all)
dwtest(m2_less)








## Regression of ETH sample
#slice data for ETH sample size 2016-2021 -> series2/sample2 for ETH
series2_eth <- data_aggr_ETH
sample2_eth <- series2_eth
sample2_eth$Close[is.na(sample2_eth$Close)] <- as.numeric(0.01)

series2_Gold <- sample1_Gold %>% slice(25:96)
sample2_Gold <- do.call(cbind.data.frame, series2_Gold)

series2_REIT <- sample1_REIT %>% slice(25:96)
sample2_REIT <-do.call(cbind.data.frame, series2_REIT)

series2_SP <- sample1_SP %>% slice(25:96)
sample2_SP <-do.call(cbind.data.frame, series2_SP)

series2_Bill <- sample1_Bill %>% slice(25:96)
sample2_Bill <- do.call(cbind.data.frame, series2_Bill)

CPI_all_2<-CPI$CPIAUCSL_PC1
CPI_all_2<-CPI_all_2[25:96]


CPI_less_food_energy_2<-CPI$CPILFESL_PC1
CPI_less_food_energy_2<-CPI_less_food_energy_2[25:96]

#define variables for regression
ETH_logreturn2<-sample2_eth$Close
Gold_logreturn2<-sample2_Gold$Closing
REIT_logreturn2<-sample2_REIT$Closing
sp500_logreturn2<-sample2_SP$Closing
bill_loginterest2<-sample2_Bill$Closing


m3_all <- lm(CPI_all_2~ETH_logreturn2+REIT_logreturn2+Gold_logreturn2+ sp500_logreturn2+bill_loginterest2)

#CPI less food & energy
m4_less <- lm(CPI_less_food_energy_2~ETH_logreturn2+Gold_logreturn2+REIT_logreturn2+ sp500_logreturn2+bill_loginterest2)


l6 <- lm(CPI_all~ETH_logreturn2)
l7 <- lm(CPI_all~Gold_logreturn2)
l8 <- lm(CPI_all~REIT_logreturn2)
l9 <- lm(CPI_all~sp500_logreturn2)
l10 <- lm(CPI_all~bill_loginterest2)
stargazer(l1,l2,l3,l4,l5, type = "html",
          title = "test", out = "test.html")

# Checking model statistics
tab_model(m3_all)
tab_model(m4_less)

summary(m3_all)
summary(m4_less)
#html table output for regression
stargazer(m3_all,m4_less, type = "html",
          title = "Regression output CPI all urban consumers", out = "eth_reg.html")

