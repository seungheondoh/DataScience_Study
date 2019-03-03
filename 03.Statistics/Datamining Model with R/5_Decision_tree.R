# Data Mining 2018-1
# Lab session for R

# 5. Decision Tree

# install and load packages
install.packages("rpart")
library(rpart)

# load dataset
data("kyphosis", package="rpart")
data <- kyphosis

# construct decision tree
DTModel <- rpart(Kyphosis ~ ., method="class", data=data)

# display the result
printcp(DTModel)
plotcp(DTModel) # plot a complexity parmeter table
summary(DTModel)

# visualize tree
plot(DTModel, uniform=TRUE, main="Classification Tree for Kyphosis")
text(DTModel, use.n=TRUE, all=TRUE, cex=.8)

