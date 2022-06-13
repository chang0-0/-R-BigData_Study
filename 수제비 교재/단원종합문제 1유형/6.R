library(dplyr)
list.files()

# churn 데이터세트에서
# TotalChanges 항목에서 이상값을 제외한 평균을 구하시오.
# (이상값은 평균에서 1.5표준편차 이상인 값으로 한다.)

main <- read.csv(
    file = "WA_Fn-UseC_-Telco-Customer-Churn.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

head(main)
colSums(is.na(main))

me <- mean(main$TotalCharges, na.rm = T)
msd <- sd(main$TotalCharges, na.rm = T)


upper <- me + msd*1.5
lower <- me - msd*1.5

temp <- main %>% filter(TotalCharges > lower & TotalCharges < upper) %>% summarise(mean = mean(TotalCharges))
temp

result6 <- temp
print(result6)
