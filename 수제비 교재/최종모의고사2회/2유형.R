library(dplyr)
library(caret)
library(e1071)
library(randomForest)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사2회")
list.files()

main <- read.csv(
    file = 'Loan.csv',
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

# 컬럼 제거
main <- subset(
    main, select = -c(paid_off_time, past_due_days)
)

str(main)
main$loan_status

main$loan_status <- ifelse(
    main$loan_status == "PAIDOFF", "True", "False"
)

main$loan_status <- as.factor(main$loan_status)
levels(main$loan_status)
main$loan_status <- relevel(main$loan_status, "True")

set.seed(2108)
parts <- sample(
    1:nrow(main), 
    size = 0.7 * nrow(main)
)

train <- main[parts, ]
test <- main[-parts, ]

set.seed(2108)
parts <- sample(
    1:nrow(train), 
    size = 0.7 * nrow(train)
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]

str(train)
pre <- preProcess(
    train[-c(1, 2), ],
    method = c("range")
)

sc.t <- predict(pre, t.train)
sc.v <- predict(pre, t.valid)

rf <- randomForest(
    loan_status ~ . -Loan_ID, 
    sc.t,
    ntree = 500,
    do.trace = TRUE
)

rf.pred <- predict(
    rf,
    newdata = sc.v
)

confusionMatrix(
    sc.v$loan_status,
    rf.pred
)

# Accuracy : 0.5755

# ==================== SVM ====================

sv <- svm(
    loan_status ~ . -Loan_ID, 
    sc.t,
)

svm.pred <- predict(
    sv,
    newdata = sc.v
)

confusionMatrix(
    sc.v$loan_status,
    svm.pred
)

# Accuracy : 0.6792


# svm의 정밀도가 더 높음

final <- svm(
    loan_status ~ . - Loan_ID, 
    train,
    probability = TRUE
)

final.pred <- predict(
    final,
    newdata = test,
    type = 'prob',
    probability = TRUE
)


list <- attr(final.pred, "probabilities")[, 2]
list

result <- data.frame(
    c(1:nrow(test)),
    list
)

names(result) <- c("index", "loan_status")
head(result)

write.csv(result, "06result.csv", row.names = FALSE)

