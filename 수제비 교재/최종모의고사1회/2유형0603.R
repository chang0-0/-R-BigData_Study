library(dplyr)
library(caret)
library(rpart)
library(e1071)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사1회")
data(iris)
main.ds <- iris

summary(main.ds)

parts <- createDataPartition(
    main.ds$Species,
    p = 0.7
)

str(main.ds)
train <- main.ds[parts$Resample1, ]
test <- main.ds[parts$Resample1, -5]

parts <- createDataPartition(
    train$Species,
    p = 0.7
)

t.train <- train[parts$Resample1, ]
t.valid <- train[parts$Resample1, ]

rp <- rpart(
    Species ~.,
    data =  t.train
)

rp.pred <- predict(
    rp,
    newdata = t.valid,
    type = 'class'
)

confusionMatrix(
    rp.pred,
    t.valid$Species
)
#  Accuracy : 0.96


svm.md <- svm(
     Species ~.,
    data =  t.train
)

svm.pred <- predict(
     svm.md,
    newdata = t.valid,
    type = 'class'
)

confusionMatrix(
    svm.pred,
    t.valid$Species
)

# Accuracy : 1


svm.md <- svm(
    Species ~.,
    data =  train
)

svm.pred <- predict(
     svm.md,
    newdata = test,
    type = 'class'
)

result <- data.frame(
    svm.pred
)

result
names(result) <- c("pred")
write.csv(result, "result0603.csv", row.names = FALSE)
