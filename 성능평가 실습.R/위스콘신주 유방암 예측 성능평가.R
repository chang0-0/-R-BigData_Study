library(mlbench)
library(C50)
library(gmodels)
library(caret)
library(Epi)
library(tidyverse)

data(BreastCancer)
df <- BreastCancer

head(df, 10)
summary(df)
str(df)

# ID열을 제거
df <- df[-1]

# 9개의 변수 factor형을 numeric형으로 바꿈
df <- cbind(lapply(df[-10], function(x) as.numeric(as.character(x))), df[10])

str(df)
df <- as.data.frame(df)

# train데이터와 test데이터 2개의 집단으로 분리
# 랜덤으로 훈련 7: 테스트 3의 비율로 분리
set.seed(202205)
samples <- sample(nrow(df), 0.7 * nrow(df))
train <- df[samples, ]
test <- df[-samples, ]

dim(train)
dim(test)


# 양성과 음성의 2개의 값을 기준으로 table형태를 생성
table(train$Class)
table(test$Class)

# 의사결정나무 사용
dctree <- C5.0(formula = Class ~ . , data = train)
dctree

dctree.pred <- predict(dctree, newdata = test, type = "class")

# 혼동 행렬 생성.
bc.C50.cmatrix <- table(test$Class, dctree.pred, dnn=c("Actual", "Predicted"))
bc.C50.cmatrix

CrossTable(test$Class, dctree.pred, prob.chisq = FALSE, dnn = c("Actual", "Predicted"))

bc.C50.cmatrix
sum(diag(bc.C50.cmatrix))/ sum(bc.C50.cmatrix)
# 같은 결과값 추출
mean(test$Class == dctree.pred)

# caret의 혼동행렬을 통한 정확도 계산
# 음성 = positive
caret::confusionMatrix(dctree.pred, test$Class, positive = "malignant")


sensitivity(dctree.pred, test$Class, positive = "malignant")
specificity(dctree.pred, test$Class, positive = "malignant")


bc.C50.pred <- predict(dctree, newdata = test, type="prob")
result <- ROC(test = bc.C50.pred[, 2], stat = test$Class, MI=FALSE, main = "ROC Curve")
result
