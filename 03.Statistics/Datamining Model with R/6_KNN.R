# Data Mining 2018-1
# Lab session for R

# 6. K-nearest neighbor

# install and load packages
install.packages("class")
library(class)

# load dataset
data(iris)
data <- iris

# convert the dependent variable to factor
data$Species <- factor(data$Species)

# split data into 3 partitions (training, validate, test)
set.seed(100) # setting seed to generate random number
train_index <- sample(1:nrow(data), 0.6*nrow(data))
train_set <- data[train_index,]
temp_set <- data[-train_index,]
validate_index <- sample(1:nrow(temp_set), 0.5*nrow(temp_set))
validate_set <- temp_set[validate_index,]
test_set <- temp_set[-validate_index,]

# generate KNN model for various number of k, then decide the best k
accuracy <- numeric() # to store the preformance at each k
for(i in 1:50){
  predicted <- knn(train_set[,-5], validate_set[,-5], train_set$Species, k = i)
  accuracy <- c(accuracy, mean(predicted==validate_set$Species))
}

# plot performance of k from 1 to 50
plot(1-accuracy, type="l", ylab="Error Rate", xlab="k", main="Error rate for iris with varying k")

# take the best k
k <- which.max(accuracy)
KNNModel <- knn(train_set[,-5], test_set[,-5], train_set$Species, k=k)

# generate confusion matrix
ConfusionMatrix <- table(KNNModel, test_set$Species)

