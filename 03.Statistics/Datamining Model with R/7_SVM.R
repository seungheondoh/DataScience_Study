# Data Mining 2018-1
# Lab session for R

# 7. Support Vector Machine

# install and load packages
install.packages("e1071")
library(e1071)

# load dataset
data(iris)
data <- iris

# convert the dependent variable to factor
data$Species <- factor(data$Species)

# split data into 2 partitions (training, test)
set.seed(100) # setting seed to generate random number
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_set <- data[train_index,]
test_set <- data[-train_index,]

# construct simple SVM model and show summary
SVMModel <- svm(Species ~ ., data=train_set)
summary(SVMModel)

# predict for the test dataset 
predicted <- predict(SVMModel, test_set)

# generate confusion matrix
ConfusionMatrix <- table(predicted, test_set$Species)


