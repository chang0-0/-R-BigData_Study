install.packages("MASS")
library("MASS")
ds <- ChickWeight
summary(ds)

min_weight <- min(ds$weight)
max_weight <- max(ds$weight)


normalize <- function(x) {
    return ((x-min(x)) / (max(x) - min(x)))
}

ds$sc_weight <- normalize(ds$weight)

ds$sc_weight <- scale(ds$weight, min_weight, max_weight - min_weight)

summary(ds$sc_weight)
print(ds$sc_weight)

library(dplyr)

# 파이프 연산자 => A를 전달 받아서 B를 처리
# weight를 최소-최대 척도(Min-Max Scaleing)로 변환한 결과가 0.5 이상인 레코드 수를 구하시오.

# 1. 데이터 세트 ds를 전달받아서 sc_weight가 0.5 이상인 값만 filtering 처리한다.
# 2. 다음 결과물로 nrow()를 처리한다. 그러면 row의 갯수가 나옴
result <- ds %>% filter(sc_weight >= 0.5) %>% nrow()

nrow(ds$sc_weight)

print(result)
