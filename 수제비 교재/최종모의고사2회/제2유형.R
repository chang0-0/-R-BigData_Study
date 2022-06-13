library(dplyr)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(mlbench)
library(ModelMetrics)
install.packages("ModelMetrics")


setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사2회")
list.files()

main.ds <- read.csv(
    file = "Loan.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE,
)

head(main.ds)
summary(main.ds)
head(main.ds$loan_status, 40)

main.ds <- main.ds %>% mutate(
    loan_status = factor(
        ifelse(
            loan_status == 'PAIDOFF',
            'Success', 'Failure'
        ),
        levels = c('Success', 'Failure')
    )
)

head(main.ds$loan_status, 20)

# paid_off_time, past_due_days 컬럼 제거
sapply(main.ds, function(x) {
    sum(is.na(x))
})


# 2개의 컬럼 제거.
main.ds <- subset(main.ds, select = -c(paid_off_time, past_due_days))
str(main.ds)


# 전체 데이터를 7:3 으로 훈련데이터와 테스트 데이터로 분리
set.seed(10)
parts <- sample(
    1:nrow(main.ds),
    size = nrow(main.ds) * 0.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]

nrow(train)
nrow(test)

# library(scales)
# scales::percent(
#     nrow(train) / nrow(main.ds)
# )

# 검증 데이터 만들어서 모델생성하기

set.seed(10)
parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]
summary(t.train)

set.seed(10)
md_svm <- svm(
    loan_status ~ . -Loan_ID,
    data = t.train,
    probability = TRUE,
)

md_svm.pred <- predict(
    object = md_svm,
    newdata = t.valid,
    pobability = TRUE
)


caret::confusionMatrix(
    data = md_svm.pred,
    refer = t.valid$loan_status
)

mean(md_svm.pred == t.valid$loan_status)

## 랜덤포레스트 모델 실행


set.seed(10)
md_rf <- randomForest(
    loan_status ~ . -Loan_ID,
    data = t.train,
    ntree = 500,
    do.trace = TRUE,
    probability = TRUE
)

md_rf 

md_rf.pred <- predict(
    object = md_rf,
    newdata = t.valid,
    probability = TRUE
)

md_rf.pred


caret::confusionMatrix(
    data = md_rf.pred,
    refer = t.valid$loan_status
)


auc(
    actual = t.valid$loan_status,
    predicted = md_rf.pred
)


mean(t.valid$loan_status == md_rf.pred)

# 의사결정나무 모델 실행


md_tree <- rpart(
    loan_status ~ . - Loan_ID,
    data = t.train,
)

md_tree_pred <- predict(
    md_tree, 
    newdata = t.valid,
    probability = TRUE,
    type = 'class'
)

md_tree_pred

caret::confusionMatrix(
    data = md_tree_pred,
    refer = t.valid$loan_status
)

auc(
    actual = t.valid$loan_status,
    predicted = md_tree_pred
)

mean(t.valid$loan_status == md_tree_pred)


# svm의 accuracy가 가장 높음

set.seed(10)
final_tree <- rpart(
    loan_status ~ . -Loan_ID,
    data = train,
    method = 'class'
)

final_tree

final_tree.pred <- predict(
    object = final_tree,
    newdata = test,
    probability = TRUE,
    type = 'class'
)


final_tree.pred

auc(
    actual = t.valid$loan_status,
    predicted = final_tree.pred
)


names(final_tree.pred) <- c("Success")
final_tree.pred$Success <- as.double(final_tree.pred$Success)
final_tree.pred$Success


result <- data.frame(
    1:nrow(final_tree.pred),
    final_tree.pred
)

names(result) <- c("index", "loan_status")
write.csv(result, "의사결정나무결과.csv", row.names = FALSE)
read <- read.csv("의사결정나무결과.csv")
head(read)


set.seed(10)
final_svm <- svm(
    loan_status ~ . -Loan_ID,
    data = train,
    probability = TRUE
)

final_pred <- predict(
    object = final_svm,
    newdata = test,
    probability = TRUE,
    type = "prob",
)



final_pred
class(final_pred)

head(attr(final_pred, "probabilities"))

prob_svm <- attr(final_pred, "probabilities")[,1]
prob_svm
