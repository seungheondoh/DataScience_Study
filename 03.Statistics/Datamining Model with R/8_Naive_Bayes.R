# Data Mining 2018-1
# Lab session for R

# 8. Naive Bayes Classifier

# install and load 
install.packages("e1071")
library(e1071)

# load dataset
data("Titanic")
Titanic_df <- as.data.frame(Titanic)

# there are 32 observations which represent all possible combinations of Class, Sex, Age, and Survived with their frequency
# create data from table
# this will repeat each combination equal to the frequency of each combination
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq)
data <- Titanic_df[repeating_sequence,]
data$Freq <- NULL

# split data into 2 partitions (training, test)
set.seed(100) # setting seed to generate random number
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_set <- data[train_index,]
test_set <- data[-train_index,]

# create Naive Bayes model and show summary
NBModel <- naiveBayes(Survived ~ ., data=train_set)
NBModel

# predict for the test dataset 
predicted <- predict(NBModel, test_set)

# generate confusion matrix
ConfusionMatrix <- table(predicted, test_set$Survived)


