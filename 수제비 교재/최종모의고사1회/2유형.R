library(rpart)
library(e1071)
library(caret)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사1회")
data(iris)
main.ds <- iris

rpart_model <- rpart(
    Species ~ .,
    data = main.ds
)

rpart_model

rpart_pred <- predict(
    rpart_model,
    newdata = main.ds,
    type = 'class'
)

rpart_pred


confusionMatrix(
    data = rpart_pred,
    refer = main.ds$Species
)

svm.model <- svm(
    Species ~ ., 
    data = main.ds
)

svm.pred <- predict(
    svm.model,
    main.ds
)

confusionMatrix(
    data =  svm.pred,
    refer = main.ds$Species
)


write.csv(svm.pred, file = "svm_pred.csv", row.names = F)


### 다른 책 예시

library(rpart)
library(e1071)
library(scales)
library(caret)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사1회")
data(iris)
main.ds <- iris

parts <- createDataPartition(
    main.ds$Species,
    p = 0.8
)

train <- main.ds[parts$Resample1, ]
test <- main.ds[-parts$Resample1, ]

dt.m <- rpart(Species ~ ., data = train)
dt.m

dt.m.pred <- predict(
    object = dt.m,
    newdata = test,
    type = 'class'
)

cm <- confusionMatrix(
    data = dt.m.pred,
    refer = test$Species
)

cm$overall[1]
mean( test$Species == dt.m.pred )

dt.svm <- svm(
    Species ~ ., data = train
)

dt.svm.pred <- predict(
    object = dt.svm,
    newdata = test,
    type = 'class'
) 

cm2 <- confusionMatrix(
    data = dt.svm.pred,
    refer = test$Species
)

#  Accuracy : 0.9333
# svm의 Accuracy가 미세하지만 더 높음

library(rpart.plot)
windows(width = 4, height = 4)

prp(dt.m, type = 4, extra = 2)

result <- dt.svm.pred

write.csv(result, "dt.svm.pred.csv", row.names = F)
