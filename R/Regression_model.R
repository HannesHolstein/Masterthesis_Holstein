library(foreign)
library(caret)
library(Metrics)
library(car)

#Data from Excel - preparation details see excel file in comments
CPI <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\CPI.csv",sep=";")
CPI_data <- do.call(cbind.data.frame, CPI)
BTC <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\btc.csv",sep=";")
ETH <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\eth.csv",sep=";")
Gold <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\gold.csv",sep=";")
REIT <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\REIT.csv",sep=";")
Six_M_Bill <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\6M_bill.csv",sep=";")
SP500 <- read.csv("C:\\Users\\Hannes Holstein\\Documents\\R\\sp500.csv",sep=";")


##NOTICE: right now regression will not work due to data fitting bugs - to be resolved asap
#OLS-regression
ols_model <- lm(CPI$CPIAUCSL_PC1~BTC$Moving.1.Month.Average + Gold$AM + REIT$Moving.1.Month.Average + Six_M_Bill$Moving.1.Month.Average +  SP500$Moving.1.Month.Average)


lm(series_CPI_df$value~series_reit_df$value + series_treasury_df + series_sp500_df + series_gold_df + coin_hist_btc + coin_hist_eth)
#CPI less food & energy
ols_CPI_LFE

# Checking model statistics
summary(ols_model)
startgazer(ols_model, type="text")

##  AIC = – 2 * ln(likelihood) + 2 * p
##  BIC = – 2 * ln(likelihood) + ln(N) * p
## p = number of estimated parameters and N = sample size.
##  AIC and BIC values can be used for choosing the best predictor subsets in regression and for comparing different models.
##  When comparing different models, the model with minimum AIC and BIC values is considered the best model.

# Using AIC function
AIC(ols_model)
# Using BIC function
BIC(ols_model)

# RMSE
names(ols_model)
rmse()

#histogram of residuals
hist(ols_model$residuals, color = "grey")
# Using plot function for NPP plot
plot(ols_model)
#VIF calculation
vif(ols_model)

#autocorrelation Durbin-Watson test
library(lmtest)
dwtest(ols_model)
