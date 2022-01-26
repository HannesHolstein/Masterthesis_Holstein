

library(caret)
# Split data into train and test
index <- createDataPartition(housing$Price, p = .70, list = FALSE)
train <- housing[index, ]
test <- housing[-index, ]


# Checking the dim of train
dim(train)


# Checking model statistics
summary(lmModel)



# Using AIC function
AIC(lmModel)
# Using BIC function
BIC(lmModel)



# Checking model object for actual and predicted values
names(lmModel)




library(Metrics)
rmse(actual = train$Price, predicted = lmModel$fitted.values)






# Histogram to check the distribution of errors
hist(lmModel$residuals, color = "grey")







plot(lmModel)







# Using plot function
plot(lmModel)
