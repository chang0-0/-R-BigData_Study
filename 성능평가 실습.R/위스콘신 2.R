library(mlbench)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)

library(scales)
library(rpart)
library(ggplot2)
library(ModelMetrics)
# library(gmodels) gmodels는 사용불가


data(BreastCancer)
main <- BreastCancer

str(main)
head(main, 10)
summary(main)
# Class의 양성과 음성을 예측.
# 예측값은 ROC나 를 통해 확인

# 환자의 ID는 제거
main <- main[,-1]
str(main)


# 9개의 변수 factor형을 모두 numeric형으로 전환
# 마지막 Class 컬럼은 제외
main <- cbind(lapply(main[-10], function(x) as.numeric(as.character(x))), main[10])
str(main)
colSums(is.na(main))

# train 데이터와 test데이터로 분리

set.seed(2020)
parts <- sample(
    nrow(main),
    size = nrow(main) * 0.7
)

train <- main[parts, ]
test <- main[-parts, ]
nrow(test)

# 데이터 분리 비율 확인
scales::percent(
    nrow(train) / nrow(main)
)

dim(train)
dim(test)


dim(train)
dim(test)

table(train$Class)
table(test$Class)

dctree <- rpart(
    Class ~ ., 
    data = train
)

windows(width = 5, height = 5)
plot(dctree, compress = TRUE, margin = 0.3)
text(dctree, cex = 0.8)

dc.p <- predict(
    dctree,
    newdata = test,
    type = 'class'
)


CM <- caret::confusionMatrix(
    dc.p,
    test$Class
)
# Accuracy : 0.9429

CrossTable(
    test$Class, 
    dc.p
)

chisq.test(dc.p, test$Class)

# 확률로 예측값을 바꿔서 ROC값 확인하기

dc.p <- predict(
    dctree,
    newdata = test,
    type = 'class'
)

head(dc.p)
list <- dc.p[,2]
list

# malignant가 긍정

library(ROCR)
plot(
    performance(
        list
    )
)

plot.roc(dc.p[,2], test$Class)

# auc 값 출력하기.
auc(test$Class, dc.p)

table(test$Class, dc.p)

CM <- caret::confusionMatrix(
    dc.p,
    test$Class,
    positive = 'malignant'
)

str(CM)
CM$byClass
print(CM$byClass[1])


data(BreastCancer)
main2 <- BreastCancer
str(main2)


idList <- main2[,1]
idList

str(main2)
main2 <- main2[, -1]
colSums(is.na(main2))
str(main2)


main2 <- cbind(lapply(main2[-9], function(x) as.numeric(as.character(x))), main2[9])
str(main2)


main2 <- BreastCancer
str(main2)

idlist <- main2[,1]

main2 <- main2[,]

main2 <- cbind(
    lapply()
)