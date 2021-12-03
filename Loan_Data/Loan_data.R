list.files()
library(dplyr)

df <- read.csv("./Loan payments data.csv")

summary(df)
colSums(is.na(df))
# 항상 첫번째는 결측값을 확인.
# 문제가 전체 데이터일 때는 결측값이 있는 컬럼은 제외

# 전체 데이터를 분리.  0.7은 train, 0.3은 test

df_70 <- nrow(df) * 0.7
df_30 <- nrow(df) * 0.3

train_set <- df[1:df_70,]
test_set <- df[df_70+1:df_70+df_30, ]

head(train_set)
head(test_set)

summary(train_set)
summary(test_set)
