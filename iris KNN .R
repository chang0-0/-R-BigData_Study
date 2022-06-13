library(tidyr)
library(dplyr)
library(caret)
library(knitr)
library(class)

data(iris)
head(iris)

set.seed(1234)

dataNorm <- iris

# Species를 제외하고 데이터 스케일링
dataNorm[, -5] <- scale(
    iris[,-5]
)

head(dataNorm)

set.seed(1234)
ind <- sample(
    2, 
    nrow(dataNorm),
    replace = TRUE, 
    prob = c(0.7, 0.3)
)

trainData <- dataNorm[ind == 1,]
testData <- dataNorm[ind == 2,]

nrow(trainData)
nrow(testData)

KnnTest1 <- knn(
    trainData[, -5],
    testData[, -5],
    trainData$Species,
    k=1,
    prob = TRUE
)

KnnTest2 <- knn(
    trainData[, -5],
    testData[, -5],
    trainData$Species,
    k=2,
    prob = TRUE
)

KnnTest3 <- knn(
    trainData[, -5],
    testData[, -5],
    trainData$Species,
    k=3,
    prob = TRUE
)

KnnTest4 <- knn(
    trainData[, -5],
    testData[, -5],
    trainData$Species,
    k=4,
    prob = TRUE
)

KnnTest5 <- knn(
    trainData[, -5],
    testData[, -5],
    trainData$Species,
    k=5,
    prob = TRUE
)

table(
    testData$Species,
    KnnTest1
)

sum(KnnTest1 == testData$Species) / length(testData$Species) * 100
sum(KnnTest2 == testData$Species) / length(testData$Species) * 100
sum(KnnTest3 == testData$Species) / length(testData$Species) * 100
sum(KnnTest4 == testData$Species) / length(testData$Species) * 100
sum(KnnTest5 == testData$Species) / length(testData$Species) * 100

KnnTestPrediction <- list()
accuracy <- as.numeric()

for(k in 1:100) {

    KnnTestPrediction[[k]] <- knn(
        trainData[,-5],
        testData[, -5],
        trainData$Species,
        k,
        prob = TRUE
    )

    accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
}

windows(width = 4, height = 4)

# Accuracy vs Choice of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)


