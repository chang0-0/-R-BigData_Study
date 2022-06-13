library(caret)
library(rpart)
library(rpart.plot)
library(party)

ds <- iris
head(ds)


# 훈련용 데이터 80% 테스트 데이터 20% 분리
parts <- createDataPartition(ds$Species, p = 0.8)
parts

train <- iris[parts$Resample1, ]
test <- iris[-parts$Resample1, ]

table(train$Species)
table(test$Species)

# 훈련용 데이터로 의사결정나무 모델 학습하기
dt.m <- rpart(Species ~ ., data = train)
dt.m

windows(width = 8, height = 8)
plot(dt.m, compress = TRUE, margin = 0.3)
text(dt.m, cex = 1.5)

if(0)`
분석결과 

Petal.Length < 2.45 경우
setosa로 분류되고,  
Petal.Length >=2.45이고 Petal.Length < 4.75의 경우 versicolor로 분류된 것을 알 수 있다.
`

# 예측

dt.m.pred <- predict(dt.m, newdata = test, type = "class")
caret::confusionMatrix(test$Species, dt.m.pred)
library(rpart.plot)
prp(dt.m, type = 4, extra = 2)
dt.m2 <- ctree(Species ~ ., data = train)

plot(dt.m2)
dt.m2.pred <- predict(dt.m2, newdata = test)
caret::confusionMatrix(test$Species, dt.m.pred)