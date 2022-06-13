library(dplyr)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사2회")
main <- read.csv(
    file = "Loan.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)


str(main)
head(main$loan_status, 10)


main$loan_status <- ifelse (
    main$loan_status == 'PAIDOFF', "Success", "Fail"
)
head(main)
main$loan_status <- as.factor(main$loan_status)
levels(main$loan_status)
main$loan_status <- relevel(main$loan_status, "Success")


main <- subset( main, select = -c(past_due_days, paid_off_time) )
str(main)

# paid_off_time과 past_due_days 컬럼을 제거하고
# 전체  데이터를 7:3으로 훈련 데이터와 테스트 데이터로 분할 하여,
# 훈련 데이터로 대출 상환 가능 여부에 대한 예측 모델을 만들고
# 테스트 데이터로 고객의 대출 상환 가능 확률을 예측하여 csv파일로 제출하시오

# 데이터 분할 시 set.seed(2108)을 이용하여 무작위 샘플링을 하고,
# 제출되는 csv의 컬럼명은 반드시 index, loan_status로 하시오

set.seed(2108)
parts <- sample(
    1:nrow(main),
    size = nrow(main) * 0.7
) 

head(main, 10)

train <- main[parts, ]
test <- main[parts, ]

set.seed(2108)
parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.7
) 

t.train <- train[parts, ]
t.valid <- train[-parts, ]


model <- preProcess(
    t.train[, -1],
    method = c('range')
)

str(sd.train)
sd.train <- predict(model, t.train)
sd.valid <- predict(model, t.valid)

library(randomForest)
rf <- randomForest(
    loan_status ~ . - Loan_ID,
    t.train    
)

rf.p <- predict(
    rf,
    newdata = sd.valid
)

rf.p
confusionMatrix(
    rf.p,
    sd.valid$loan_status
)

# Accuracy : 0.6887

sv <- svm(
    loan_status ~ . - Loan_ID,
    t.train 
)

sv.p <- predict(
    sv,
    newdata = sd.valid
)

confusionMatrix(
    sv.p,
    sd.valid$loan_status
)

#  Accuracy : 0.6792

# 랜덤포레스트

final <- randomForest(
    loan_status ~ . - Loan_ID,
    train,
    do.trace = TRUE,
    ntree= 300,
    probability = TRUE
)


final.p <- predict(
    final,
    test,
    type = "prob",
    probability = TRUE,
)

final.p[,1]
head(final.p)



n <- c(1:nrow(test))
list <- final.p[,1]
list

result <- data.frame(
    n,
    list
)

head(result)
names(result) <- c("index", "loan_status")

write.csv(
    result,
    "result0610.csv",
    row.names = FALSE
)



# svm

set.seed(2108)
final2 <- e1071::svm(
    loan_status ~ . - Loan_ID,
    data = train,
    probability = TRUE
)

final2.p <- predict(
    final2,
    test,
    type = 'prob',
    probability = TRUE
)

final2.p
head(attr(final2.p, "probabilities"))

attr(final2.p, "probabilities")[,2]
