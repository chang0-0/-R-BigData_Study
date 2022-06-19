library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사2회")
list.files()

main <- read.csv(
    file = "Loan.csv",
    encoding = 'UTF-8',
    stringsAsFactor = TRUE
)

head(main)
str(main)
main <- subset(main, select = -c(paid_off_time, past_due_days))
main$loan_status <- factor(
    ifelse(
        main$loan_status == 'PAIDOFF',
        main$loan_status <- 'Success',
        main$loan_status <- 'Fail'
    ),
    levels = c('Success', 'Fail')
)

set.seed(2108)
parts <- sample(
    1:nrow(main),
    size = nrow(main) * 0.7
)

train <- main[parts, ]
test <- main[-parts, -2]

summary(train)
model <- preProcess(
    train[, -c(1)],
    method = c("range")
)
train <- predict(
    model,
    train
)

summary(test)
model <- preProcess(
    test[, -c(1)],
    method = c("range")
)
test <- predict(
    model,
    test
)


set.seed(2108)
rf <- randomForest(
    loan_status ~ . -Loan_ID,
    train,
    do.trace = TRUE,
    ntree = 400
)

pred <- predict(
    rf,
    test,
    type = 'class'
)

list <- pred[,1]
length(list)
result <- data.frame(
    c(1:length(list)),
    list
)
auc(rf)

head(result)
names(result) <- c("index", "loan_status")
write.csv(result, "result0613.csv", row.names = F)

result.t <- read.csv(
    "result0613.csv"
)

head(result.t)
