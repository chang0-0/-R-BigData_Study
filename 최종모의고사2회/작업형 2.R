setwd("C:/Users/Samsung/Desktop/빅분기실기준비/최종모의고사2회")

main.ds <- read.csv(
    "Loan.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE
)

head(main.ds)

# paid_off_time, past_due_days 컬럼 제거
main.ds <- subset(
    main.ds,
    select = -c(past_due_days, paid_off_time)
)

main.ds$loan_status

main.ds$loan_status <- factor(
    ifelse (
        main.ds$loan_status == "PAIDOFF", 
        main.ds$loan_status <- "Success", 
        main.ds$loan_status <- "Fail"
    ),
    levels = c("Success", "Fail"),
    labels = c("Success", "Fail")
)

main.ds$loan_status
levels(main.ds$loan_status)

# main.ds$loan_status <- ifelse (
#     main.ds$loan_status == "PAIDOFF", 
#     main.ds$loan_status <- "Success", 
#     main.ds$loan_status <- "Fail"
# )


main.ds$loan_status <- as.factor(main.ds$loan_status)
str(main.ds)

# 데이터를 7:3으로 분리

set.seed(2108)
parts <- sample(
    nrow(main.ds),
    size = nrow(main.ds) * 0.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]

summary(train)

# 대출 상환 가능 = loan_status = PAIDOFF 확률을 예측


# train을 다시 7:3으로 분리해서 검증데이터 생성

set.seed(2108)

valid.idx <- sample(
    nrow(train),
    size = nrow(train) * 0.7
)

valid.train <- train[valid.idx, ]
valid.valid <- train[-valid.idx, ]


library(e1071)
set.seed(2108)
md_svm <- svm(
    loan_status ~ . - Loan_ID,
    data = valid.train,
    probability = TRUE
)

set.seed(2108)
pred_svm <- predict(
    md_svm,
    newdata = valid.valid,
    probability = TRUE
)

svmCM <- caret::confusionMatrix(
    data = pred_svm,
    refer = valid.valid$loan_status,
)

set.seed(2108)
library(randomForest)
md_rf <- randomForest(
    loan_status ~ .-Loan_ID, 
    data = valid.train,
    probability = TRUE
)


md_rf.pred <- predict(
    object = md_rf,
    newdata = valid.valid,
    probability = TRUE
)

rfCM <- caret::confusionMatrix(
    data = md_rf.pred,
    refer = valid.valid$loan_status
)

rfCM$overall[1]
svmCM$overall[1]
# svm이 더 높음

set.seed(2108)
md_final <- svm(
    loan_status ~ . - Loan_ID,
    train, 
    probability = TRUE
)

final_pred <- predict(
    object = md_final,
    newdata = test,
    probability = TRUE,
    type = "response"
)

final_pred

finalCM <- caret::confusionMatrix(
    final_pred,
    test$loan_status,
)

head(attr(final_pred, "probabilities"))
prob_svm <- attr(final_pred, "probabilities")[,1]

df_result <- data.frame(
    c(1:nrow(test)),
    prob_svm
)

colnames(df_result) <- c("index", "loan_status")
head(df_result, 40)

write.csv(df_result, "result.csv", row.names = FALSE)
check <- read.csv("result.csv") 
head(check)
