# Data Mining 2018-1
# Lab session for R

# 2. Logistic regression

# load dataset
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# convert rank to a factor to indicate that rank should be treated as a categorical variable
data$rank <- factor(data$rank)

# partition data into training and test dataset
set.seed(100) # setting seed to generate random number
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_set <- data[train_index,]
test_set <- data[-train_index,]

# build logistic model
LogisticModel <- glm(admit ~ gre + gpa + rank, data=train_set, family=binomial(link="logit"))

# dist = Intercept + (beta * speed)
summary(LogisticModel)

# predict probability scores (from 0 to 1) with logistic model
predicted <- predict(LogisticModel, test_set, type="response")

# decide on optimal prediction probability cutoff for the model
install.packages("InformationValue")
library(InformationValue)
cutoff <- optimalCutoff(test_set$admit, predicted)[1]

# calculate misclassification error
error <- misClassError(test_set$admit, predicted, threshold=cutoff)

# plot ROC curve
plotROC(test_set$admit, predicted)

