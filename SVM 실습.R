library(ggplot2)
library(e1071)

data(iris)

iris.sub <- subset(iris, select = c("Sepal.Length", "Sepal.Width", "Species" ),
    subset = Species %in% c("setosa", "virginica"))
iris.sub$Species <- factor(iris.sub$Species)
head(iris.sub)
tail(iris.sub)
windows(height = 8, width = 8)

ggplot(iris.sub, aes(x=Sepal.Length, y=Sepal.Width)) + 
    geom_point(aes(color=Species, shape = Species), size=2)

set.seed(123)
iris.svm <- svm(Species ~ . , data = iris.sub, 
kernel = "linear", cost = 1, scale = FALSE)

summary(iris.svm)

library(AER)
data("Affairs")
str(Affairs)

# affairs가 수치형 데이터이기 때문에 SVM에서 사용하기 위해서 이진데이터로 변환

# factor로 변환 한번이라도 있으면 1, 한번도 없으면 0으로 표시
# 이후에 factor형에서 leleves는 "No", "Yes"로 구분

## 방법 1.
aff$affairs <- factor(ifelse(aff$affairs > 0, 1, 0), levels = c(0, 1),
    labels = c("No", "Yes")
)

## 방법 2.
aff$affairs <- ifelse( aff$affairs > 0, aff$affairs <- "Yes", aff$affairs <- "No" )
aff$affairs <- as.factor(aff$affairs)



str(aff)
table(aff$affairs)
prop.table(table(aff$affairs))

set.seed(123)
parts <- sample(nrow(aff) , 0.7 * nrow(aff))
aff.train <- aff[parts, ]
aff.test <- aff[-parts, ]

nrow(aff.train)
nrow(aff.test)
table(aff.train$affairs)
table(aff.test$affairs)

set.seed(123)
aff.svm <- svm(affairs ~ . , data = aff.train)
summary(aff.svm)

aff.svm.pred <- predict(aff.svm, newdata = aff.test)

table(aff.test$affairs, aff.svm.pred, dnn=c("Actual", "Predicted"))
mean(aff.test$affairs == aff.svm.pred)

aff.svm2 <- svm(affairs ~ ., data = aff.train, probability = TRUE)
aff.svm.pred2 <- predict( aff.svm2, newedata = aff.test, probability = TRUE )

summary(aff.svm.pred2)

