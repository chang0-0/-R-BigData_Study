data(iris)
ds <- iris
head(ds)
colSums(is.na(ds))
nrow(ds)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/KNN 제2유형")

# iris의 데이터셋을 이용하여 각 데이터에 따른 종의 분류를 시행(KNN)해보고 이에 대한 결과에 대해서 
# K=10, 일 때 혼동행렬 이용 정확도 및 결과
# K=60, 일 때 혼동행렬 이용 정확도 및 결과를 보이시오

# 조건 : 
# 데이터는 무작위화하여 사용
# 총 150개 데이터 중 130개를 학습에 사용 나머지 20개를 이용해 test한 결과를 나타냄
# 분석 전에 모든 숫자 컬럼에 대한 Min-Max 정규화를 수행

set.seed(123)
n <- 1:150

parts <- sample(
    1:nrow(ds),
    size = 150
)

iris_random1 <- iris[order(parts), ]
head(iris_random1)


random <- runif(150)
iris_random2 <- iris[order(random), ]
head(iris_random2)


normal <- function(x) ( 
    return(
        (x-min(x)) / (max(x) - min(x))
    )
)
normal(1:5)

iris_new <- as.data.frame(
    lapply(iris_random[, -5], normal)
)
summary(iris_new)
head(iris_new)


train <- iris_new[1:130, ]
test <- iris_new[131:150, ]
train_sp <- iris_random[1:130, 5]
test_sp <- iris_random[131:150, 5]

library(caret)
library(class)

model <- knn(
    train = train,
    test = test,
    cl=train_sp, 
    k = 10
)
table(factor(model))
table(test_sp, model)
CM = confusionMatrix(
    test_sp, model
)
accuracy = CM$overall[1]

model <- knn(
    train = train,
    test = test,
    cl = train_sp, 
    k = 60
)
table(factor(model))
table(test_sp, model)
CM = confusionMatrix(
    test_sp, model
)
accuracy = CM$overall[1]


CM
accuracy
