library(dplyr)

# iris 데이터에서 
# Species가 virginica인 항목에서 Sepal.Length가 6보다 크면, 1, 아니면 0으로 
# 파생 컬럼 Len을 생성 후 Len컬럼의 sum 값을 구하시오.
data(iris)

main <- iris %>% filter(Species == "virginica")

main$Len <- NA
main$Len <- ifelse(main$Sepal.Length > 6.0, main$Len <- 1, main$Len <- 0)
main$Len

result16 <- sum(main$Len)
print(result16)