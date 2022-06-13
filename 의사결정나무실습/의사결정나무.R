library(mlbench)
data(BreastCancer)
main.ds <- BreastCancer
summary(main.ds)

# Id 컬럼 제거
bc <- main.ds[-1]

# Class 컬럼을 제외한 나머지는 수치형으로 변경
bc <- cbind(
    lapply(
        bc[-10],
        function(x) as.numeric(as.character(x))), 
        bc[10]
)

summary(bc)
str(bc)
library(rpart)

set.seed(10)
parts <- sample(
    nrow(bc),
    size = nrow(bc) * 0.7
)

train <- bc[parts, ]
test <- bc[-parts, ]

table(train$Class)

# 범주형 변수로 간주 => class
dc.tree <- rpart(
    Class ~ .,
    data = train,
    method = "class",
    parms = list(split = "information")
)

library(rpart.plot)
windows(height = 8, width = 9)
prp(dc.tree, type = 2, 
    extra = 104,
    fallen.leaves = FALSE,
    roundint = TRUE,
    main = "Decision TREE from Wisconsin Breast Cancer Dataset"
)

# type = 'prob' -> 행렬로 각 확률값을 볼 수 있음
bc.tree.pred <- predict(
    dc.tree,
    newdata = test, 
    type = 'prob',
)

bc.tree.pred

# 분리된 결과가 나옴
bc.tree.pred2 <- predict(
    dc.tree,
    newdata = test, 
    type = 'class',
)


table(
    test$Class, 
    bc.tree.pred2,
    dnn = c("Actual", "Predicted")
)

printcp(dc.tree )