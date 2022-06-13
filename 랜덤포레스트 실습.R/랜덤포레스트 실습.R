library(mlbench)
library(randomForest)

data(BreastCancer)
df <- BreastCancer
head(df)

# ID 변수 제거
df <- df[-1]
str(df)

# factor형을 숫자형으로 변경
df <- cbind(lapply(df[-10], function(x) as.numeric(as.character(x))), df[10])
str(df)

set.seed(2022)
train <- sample(nrow(df), 0.7*nrow(df) )

 # train (학습 데이터)이 전체의 70% 
df.train <- df[train, ]
 # test (검정 데이터)가 나머지 30% 
df.test <- df[-train, ]

dim(df.train)
dim(df.test)
set.seed(123)


df.forest <- randomForest(Class ~ . , data = df.train, na.action = na.roughfix , importance = TRUE)

# na.action = 결측값 처리 방법 지정 
# 코드에서 쓰인 na.roughfix는 결측값이 발생한 변수가 수치형 변수일 경우 결측값을 해당열의 중위수로 대체
# 명목형 변수일 경우, 가장 많은 빈도를 갖는  범주값으로 대체

#  importance = TRUE로 지정을 하면 랜덤포레스트를 수행 후
# 결과에서 어떤 변수가 중요한지 중요한 변수를 찾는 추가적인 분석도 가능
# 예측 변수의 중요도를 평가 할 수 있다.

df.forest
df.forest.pred <- predict(df.forest, newdata = df.test, type = "prob")
table(df.forest.pred)


dim(df.forest.pred)


library(cluster)
cluster(x = na.omit(bc.test[, -10]), clus = na.omit(bc.)  )
