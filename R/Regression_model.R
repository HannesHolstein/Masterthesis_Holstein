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

setwd("C:\\Users\\Holstein\\Documents\\R\\Projects\\Masterthesis_Holstein\\Data")

#Data from Excel - preparation details see comments on files

CPI <- read.csv(file ="CPI.csv",sep=";")
CPI_data <- do.call(cbind.data.frame, CPI)
BTC <- read.csv(file ="btc.csv",sep=";")
ETH <- read.csv(file="eth.csv",sep=";")
Gold <- read.csv(file="gold.csv",sep=";")
REIT <- read.csv(file="6M_bill.csv",sep=";")
SP500 <- read.csv(file="sp500.csv",sep=";")
Six_M_Bill <- read.csv(file="6m_bill.csv", sep=";")

##Regressions for different CPI's used
#Define variable names for regression table output
CPI_all <- CPI$CPIAUCSL_PC1
CPI_less_food_energy<- CPI$CPILFESL_PC1
BTC_sample<-BTC$Moving.1.Month.Average
Gold_sample<-Gold$AM
REIT_sample<-REIT$Moving.1.Month.Average
Treasury_bill_6m_sample<-Six_M_Bill$Moving.1.Month.Average
SP500_sample<-SP500$Moving.1.Month.Average

#plots
rdate <- as.Date(BTC$ï..Month,"%mm/%yyyy")
fix(rdate)
plot(BTC$Moving.1.Month.Average~rdate)

ggplot(BTC, mapping=
         aes(x=Month,y=Moving.1.Month.Average, group=1)) + geom_line()


#OLS-regression CPI all consumer
lm(CPI$CPIAUCSL_PC1~BTC$Moving.1.Month.Average + Gold$AM + REIT$Moving.1.Month.Average + Six_M_Bill$Moving.1.Month.Average +  SP500$Moving.1.Month.Average)
m1_all <- lm(CPI_all~BTC_sample+Gold_sample+REIT_sample+Treasury_bill_6m_sample+SP500_sample)

#CPI less food & energy
lm(CPI$CPILFESL_PC1~BTC$Moving.1.Month.Average + Gold$AM + REIT$Moving.1.Month.Average + Six_M_Bill$Moving.1.Month.Average +  SP500$Moving.1.Month.Average)
m2_less <- lm(CPi_less_food_energy~BTC_sample+Gold_sample+REIT_sample+Treasury_bill_6m_sample+SP500_sample)

# Checking model statistics
tab_model(m1_all)
tab_model(m2_less)

summary(m1_all)
summary(m2_less)
#html table output for regression
stargazer(m1_all,m2_less, type = "html",  #use html output to match planned R Markdown output
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
plot(m1_all)

par(mfrow=c(2,2))
plot(m2_less)

#VIF calculation
vif(m1_all)
vif(m1_less)

#autocorrelation Durbin-Watson test
library(lmtest)
dwtest(m1_all)
dwtest(m2_less)
