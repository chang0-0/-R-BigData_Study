library(dplyr)
library(caret)
library(rpart)
library(e1071)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사1회")

data(iris)
str(iris)

rp <- rpart(
    Species ~ . ,
    data = iris
)

rp.p <- predict(
    rp,
    newdata = iris,
    type = 'response'
)

rp.p

confusionMatrix(
   data = rp.p,
    refer = iris$Species
)

#  Accuracy : 0.96

sv <- svm(
    Species ~ .,
    data = iris
)

sv.p <- predict(
    sv,
    newdata = iris,
    type = 'class'
)

confusionMatrix(
    sv.p,
    refer = iris$Species
)

# Accuracy : 0.9733

write.csv(
    sv.p,
    "result0610.csv",
    row.names = FALSE
)