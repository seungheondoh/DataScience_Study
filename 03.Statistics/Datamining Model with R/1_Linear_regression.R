# Data Mining 2018-1
# Lab session for R

# 1. Linear regression

# load dataset
data(cars)
data <- cars

# visualize linear relationship between distance and speed
scatter.smooth(x=data$speed, y=data$dist, main="Dist ~ Speed")

# partition data into training and test dataset
set.seed(100) # setting seed to generate random number
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_set <- data[train_index,]
test_set <- data[-train_index,]

# build linear model
LinearModel <- lm(dist ~ speed, data=train_set)

# dist = Intercept + (beta * speed)
summary(LinearModel)

# predict distance with linear model
predicted <- predict(LinearModel, test_set)

# calculate root-mean-square error
RMSE <- sqrt(mean((predicted - test_set$dist)^2))

