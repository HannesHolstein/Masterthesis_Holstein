library(foreign)
library(caret)
library(Metrics)
library(car)


##NOTICE: right now regression will not work due to data fitting bugs - to be resolved asap
#OLS-regression
ols_model <- lm(series_CPI_df$value~series_reit_df$value + series_treasury_df + series_sp500_df + series_gold_df + coin_hist_btc + coin_hist_eth)





# Checking model statistics
summary(ols_model)
startgazer(ols_model, type="text")

##  AIC = – 2 * ln(likelihood) + 2 * p
##  BIC = – 2 * ln(likelihood) + ln(N) * p
## p = number of estimated parameters and N = sample size.
##  AIC and BIC values can be used for choosing the best predictor subsets in regression and for comparing different models.
##  When comparing different models, the model with minimum AIC and BIC values is considered the best model.

# Using AIC function
AIC(lmModel)
# Using BIC function
BIC(lmModel)

# RMSE
names(ols_model)
rmse(actual = train$Price, predicted = lmModel$fitted.values)

#histogram of residuals
hist(ols_model$residuals, color = "grey")
# Using plot function for NPP plot
plot(ols_model)
#VIF calculation
vif(ols_model)

#autocorrelation Durbin-Watson test
library(lmtest)
dwtest(ols_model)
