# Data Mining 2018-1
# Lab session for R

# 4. PCA

# install and load packages
install.packages("ggplot2")
library(ggplot2)
library(scales)

# load dataset
data(iris)
data <- iris[,-5]

# perform PCA
PCAModel <- prcomp(data, center=TRUE, scale=TRUE)

# summarize PCA result
summary(PCAModel)

# generate a bar plot, line plot of PCA
screeplot(PCAModel, type="barplot")
screeplot(PCAModel, type="line")

# compute the amount of the between-group variance that is explained by each principal component
prop = PCAModel$sdev^2/sum(PCAModel$sdev^2)

# visualize PCA model
dataset = data.frame(species = iris[,"Species"], pca = PCAModel$x)
plt <- ggplot(dataset) + 
  geom_point(aes(pca.PC1, pca.PC2, colour = species, shape = species), size = 2.5) + 
  labs(x = paste("PC1 (", percent(prop[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop[2]), ")", sep=""))

plt
