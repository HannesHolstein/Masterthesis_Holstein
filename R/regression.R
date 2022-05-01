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

setwd("C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data\\aggregated")


#Data from Excel - preparation done in excel

CPI <- read.csv(file ="CPI.csv",sep=";")
CPI$Month <- mdy(CPI$Month)

BTC <- read.csv(file ="btc.csv",sep=";" )
BTC$Month <- mdy(BTC$Month)

ETH <- read.csv(file="eth.csv",sep=";")
ETH$Month <- mdy(ETH$Month)


###Warning message:
###All formats failed to parse. No formats found.

Gold <- read.csv(file="gold.csv",sep=";")
Gold$Month <- mdy(Gold$Month)

REIT <- read.csv(file="REIT.csv",sep=";")
REIT$ï..Month <- dmy(REIT$ï..Month)

SP500 <- read.csv(file="sp500.csv",sep=";")
SP500$Month <- mdy(SP500$Month)

Six_M_Bill <- read.csv(file="6m_bill.csv", sep=";")
Six_M_Bill$Month <- mdy(Six_M_Bill$Month)


##plots
#CPI plot

ggplot(CPI, aes(Month)) +
  geom_line(aes(y = CPIAUCSL_PC1), color = "blue") +
  geom_line(aes(y = CPILFESL_PC1), color = "red") +
  ggtitle("CPI change for all consumers/less food energy")+
  labs(subtitle="Percent change from year ago",color = "Legend")+
  scale_color_manual(values = colors)
#BTC plot
ggplot(BTC, mapping=aes(x=Month,y=Moving.1.Month.Average,group=1))+
  geom_line() +
  ggtitle("BTC monthly average price in USD") +
  scale_y_continuous(name="1-Month-average", breaks = c(0,1000,5000,10000,15000,20000,25000,30000,35000,40000,45000,50000,55000,60000))+
  theme_bw()
#Gold plot
ggplot(Gold, mapping=aes(x=Month,y=AM,group=1))+
  geom_line() +
  ggtitle("Gold monthly average price in USD") +
  scale_y_continuous(name="1-Month-average")+
  theme_bw()
#ETH plot
ggplot(ETH, mapping=aes(x=ï..Month,y=Moving.1.Month.Average,group=1))+
  geom_line() +
  ggtitle("ETH monthly average price in USD") +
  scale_y_continuous(name="1-Month-average")+
  theme_bw()
#REIT
ggplot(REIT, mapping=aes(x=ï..Month,y=Moving.1.Month.Average,group=1))+
  geom_line() +
  ggtitle("Wilshire REIT index monthly average") +
  scale_y_continuous(name="1-Month-average")+
  theme_bw()
#SP500
ggplot(SP500, mapping=aes(x=Month,y=Moving.1.Month.Average,group=1))+
  geom_line() +
  ggtitle("S&P500 monthly average") +
  scale_y_continuous(name="1-Month-average")+
  theme_bw()
#6m Bill plot
ggplot(Six_M_Bill, mapping=aes(x=Month,y=Moving.1.Month.Average,group=1))+
  geom_line() +
  ggtitle("6-Months T-bill yield monthly average") +
  scale_y_continuous(name="1-Month-average")+
  theme_bw()


##Regressions for different CPI's used
#Define variable names for regression table output
CPI_all <- CPI$CPIAUCSL_PC1
CPI_less_food_energy<- CPI$CPILFESL_PC1
BTC_sample<-BTC$Moving.1.Month.Average
Gold_sample<-Gold$AM
REIT_sample<-REIT$Moving.1.Month.Average
Treasury_bill_6m_sample<-Six_M_Bill$Moving.1.Month.Average
SP500_sample<-SP500$Moving.1.Month.Average

#OLS-regression CPI all consumer
lm(CPI$CPIAUCSL_PC1~BTC$Moving.1.Month.Average + Gold$AM + REIT$Moving.1.Month.Average + Six_M_Bill$Moving.1.Month.Average +  SP500$Moving.1.Month.Average)
m1_all <- lm(CPI_all~BTC_sample+Gold_sample+REIT_sample+Treasury_bill_6m_sample+SP500_sample)

#CPI less food & energy
lm(CPI$CPILFESL_PC1~BTC$Moving.1.Month.Average + Gold$AM + REIT$Moving.1.Month.Average + Six_M_Bill$Moving.1.Month.Average +  SP500$Moving.1.Month.Average)
m2_less <- lm(CPI_less_food_energy~BTC_sample+Gold_sample+REIT_sample+Treasury_bill_6m_sample+SP500_sample)

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

#Residual sum of squares:
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

#Statistically, MSE is the maximum likelihood estimator of residual variance, but is biased (downward).
#The Pearson one is the restricted maximum likelihood estimator of residual variance, which is unbiased



#histogram of residuals
hist(m1_all$residuals, color = "grey")
hist(m2_less$residuals, color = "grey")

# Using plot function for NPP plot
par(mfrow=c(2,2))
plot(m1_all,main="m1_all")

par(mfrow=c(2,2))
plot(m2_less,main="m2_less")

#VIF calculation
vif(m1_all)
vif(m1_less)

#autocorrelation Durbin-Watson test
library(lmtest)
citation("lmtest")
dwtest(m1_all)
dwtest(m2_less)



