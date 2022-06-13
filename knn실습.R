library(class)
library(caret)
library(foreach)
library(scales)
library(gmodels)

data(iris)
set.seed(101)

n <- nrow(iris)
parts <- sample(
    1:n,
    size = n * 2/3,
    replace = FALSE
)

iris.train <- iris[parts, -5]
iris.test <- iris[-parts, -5]



trainLabels <- iris[parts, 5]
testLabels <- iris[-parts, 5]


colSums(is.na(iris.train))
colSums(is.na(iris.test))

knn.pred <- knn(train = iris.train, cl = trainLabels, test = iris.test, k = 5)
knn.pred
