#OLS-regression model
lm(series_CPI_df$value~series_reit_df$value + series_treasury_df + series_sp500_df + series_gold_df + coin_hist_btc + coin_hist_eth)



library(caret)

# Checking model statistics
summary(lmModel)
# Using AIC function
AIC(lmModel)
# Using BIC function
BIC(lmModel)


library(Metrics)
rmse(actual = train$Price, predicted = lmModel$fitted.values)

hist(lmModel$residuals, color = "grey")
plot(lmModel)
# Using plot function
plot(lmModel)
