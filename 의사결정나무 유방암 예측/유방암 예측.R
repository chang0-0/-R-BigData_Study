library(mlbench)
library(C50)
library(gmodels)
library(Epi)

data(BreastCancer)
df <- BreastCancer

# ID 컬럼제거
bc <- df[-1]

# 9개의 변수 factor형을 numeric형으로 바꿈
bc <- cbind(lapply(bc[-10], function(x) as.numeric(as.character(x))), bc[10])
str(bc)


# train데이터와 test데이터 2개의 집단으로 분리

# 랜덤으로 훈련 7: 테스트 3의 비율로 분리
set.seed(2022)
train <- sample(nrow(bc), 0.7 * nrow(bc))
bc.train <- bc[train, ]
bc.test <- bc[-train, ]

# 양성과 음성의 2개의 값을 기준으로 table형태를 생성
table(bc.train$Class)
table(bc.test$Class)

# 의사결정나무 사용
bc.C50 <- C5.0(formula = Class ~ . , data = bc.train)

bc.C50.pred <- predict(bc.C50, newdata = bc.test, type = "class")
bc.C50.pred

# 혼동 행렬 생성.
bc.C50.cmatrix <- table(bc.test$Class, bc.C50.pred, dnn=c("Actual", "Predicted"))
bc.C50.cmatrix

CrossTable(bc.test$Class, bc.C50.pred, prob.chisq = FALSE, dnn = c("Actual", "Predicted"))


bc.C50.cmatrix
sum(diag(bc.C50.cmatrix))/ sum(bc.C50.cmatrix)
# 같은 결과값 추출
mean(bc.test$Class == bc.C50.pred)

# caret의 혼동행렬을 통한 정확도 계산
library(caret)
# 음성 = positive
caret::confusionMatrix(bc.C50.pred, bc.test$Class, positive = "malignant")

sensitivity(bc.C50.pred, bc.test$Class, positive = "malignant")
specificity(bc.C50.pred, bc.test$Class, positive = "malignant")

bc.C50 <- C5.0(formula = Class ~ . , data = bc.train)
bc.C50.pred <- predict(bc.C50, newdata = bc.test, type="prob")


# ROC커브
ROC(test = bc.C50.pred[, 2], stat = bc.test$Class, MI=FALSE, main = "ROC Curve (Epi Package)")

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/성능평가 실습.R/")
