library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/20220516")
ds <- read.csv("Loandata.csv", stringsAsFactors = FALSE)

head(ds)
str(ds)

# 1. paid_off_time, pats_due_datys 컬럼 제거
ds <- subset(ds, select = -c(paid_off_time, past_due_days))
head(ds)

# 결측값 확인
sapply( ds, function(x) {
    sum(is.na(x))
})

# 결측값 없음.
str(ds)
head(ds)

# 훈련 데이터 7 테스트 데이터 3 으로 분리
# 데이터 분할 시 set.seed(2108)을 이용해서 
# 무작위 샘플링을 하여라


ds$term <- as.factor(as.numeric(as.character(ds$term)))
ds$Gender <- as.factor(as.character(ds$Gender))
ds$loan_status <- factor( ifelse(ds$loan_status == "PAIDOFF", "Success", "Failure") )
str(ds)

head(ds$loan_status)
levels(ds$loan_status)

set.seed(2108)
nrow(ds)

parts <- sample(nrow(ds), 0.7*nrow(ds))
parts

# 위랑 똑같이 7:3으로 분리됨
parts2 <- sample( 1:nrow(ds), size = nrow(ds) * 0.7)
parts2

train <- ds[parts2, ]
test <- ds[-parts2, ]

set.seed(2108)
nrow(train)
nrow(test)

dim(train)


levels(ds$Gender)
levels(ds$term)
str(ds)

set.seed(2108)
ds.forest <- randomForest(loan_status ~ . - Loan_ID  , data = train , probability = TRUE)
ds.forest

df.pred <- predict(ds.forest, newdata = test, probability = TRUE)
df.pred

head(attr(df.pred, "probabilities"))

caret::confusionMatrix(data = df.pred, reference = test$loan_status)



result <- data.frame(c(1:nrow(test)), df.pred)
colnames(result) <- c("index", "loan_status")
head(result)


write.csv(result, "000.csv", row.names = FALSE)
result <- read.csv("000.csv")
head(result)

# validation 데이터 생성
tr_rows <- nrow(train)
tr_idx <- sample(c(1:tr_rows), tr_rows * 0.7)

dt_loan_tr <- train[tr_idx, ]
dt_loan_val <- train[-tr_idx, ]

nrow(dt_loan_tr)
nrow(dt_loan_val)


# 책 비교
# 아래는 책 코드
ds <- read.csv("Loandata.csv")
summary(ds)

sapply(ds, function(x) {
    sum(is.na(x))
})

ds <- ds %>% mutate(loan_status = factor(
    ifelse(loan_status == "PAIDOFF", "Success", "Failure"), 
    levels = c("Success", "Failure")
))

str(ds)

# 나머지 character형 factor형으로 전환
ds <- ds %>% mutate(
    education = factor(education),
   Gender = factor(Gender)
)

str(ds)
ds <- subset(ds, select = -c(past_due_days, paid_off_time))

# 7:3으로 데이터 분리

set.seed(2108)
rows <- nrow(ds)

idx <- sample(c(1:rows), rows*0.7)

train <- ds[idx, ]
test <- ds[-idx, ]

# validation 데이터 생성
tr_rows <- nrow(train)
tr_idx <- sample(c(1:tr_rows), tr_rows * 0.7)

dt_loan_tr <- train[tr_idx, ]
dt_loan_val <- train[-tr_idx, ]

nrow(dt_loan_tr)
nrow(dt_loan_val)

set.seed(2108)


md_svm <- e1071::svm(loan_status ~ . - Loan_ID, data = dt_loan_tr, probability = TRUE)
md_svm

pred_svm <- predict(object = md_svm, newdata = dt_loan_val, probability = TRUE)

caret::confusionMatrix(data = pred_svm, reference = dt_loan_val$loan_status)

set.seed(2108)
md_final <- e1071::svm(loan_status ~ . -Loan_ID, data = test, probability = TRUE)
md_final

pred_final <- predict(object = md_final, newdata = test, probability = TRUE)
pred_final
head(attr(pred_final, "probabilities"))

prob_svm <- attr(pred_final, "probabilities")[, 1]

df_result <- data.frame(c(1:nrow(test)), prob_svm)

# 컬럼명은 반드시 index, loan_status로 하시오.
colnames(df_result) <- c("index", "loan_status")

head(df_result)

write.csv(df_result, "pred_SVM.csv", row.names = FALSE)
result <- read.csv("pred_SVM.csv")
head(result)
