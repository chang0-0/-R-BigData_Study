data(iris)
ds <- iris
summary(ds)

# 의사결정나무 rpart 패키지 추가
library(rpart)


rpart.model <- rpart(Species ~ ., data = iris)
rpart.model

# ls = list로 변수 목록 확인
ls(rpart.model)

# rpart.model 검정을 위한 cptable을 확인한다.
rpart.model$cptable

tree_pred <- predict(rpart.model, newdata = iris, type="class")

library(caret)
confusionMatrix(tree_pred, reference = iris$Species)

install.packages("e1071")
library(e1071)

svm.model <- svm(Species ~., data = iris)
svm.pred <- predict(svm.model, iris)

confusionMatrix(svm.pred, reference = iris$Species)

write.csv(svm.pred, file = "file.csv")
