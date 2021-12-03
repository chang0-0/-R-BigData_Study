list.files()
library(dplyr)

df <- read.csv("./Loan payments data.csv")

summary(df)
colSums(is.na(df))
# 항상 첫번째는 결측값을 확인.
# 문제가 전체 데이터일 때는 결측값이 있는 컬럼은 제외

# 전체 데이터를 분리.  0.7은 train, 0.3은 test

dt_loan <- df %>% mutate(Loan_ID = factor(Loan_ID), loan_status = factor(ifelse(loan_status == "PAIDOFF", "Success", "Failure"), levels = c("Success", "Failure")),effective_data = factor(effective_date), due_data = factor(due_date), 
+ paid_off_time = factor()))