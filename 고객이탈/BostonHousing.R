library(dplyr)
library(MASS)

data(Boston)
ds <- Boston

# sort는 dataframe에서는 사용할 수 없기 때문에 order()를 사용함.
# arrange()는 order() 대신 사용가능
ds <- ds[ order(-ds$medv), ]
ds$medv[c(1:50)] <- min(ds$medv[c(1:50)])
ds$medv
head(ds)

# medv컬럼 기준으로 정렬하고, 
# 상위 50개의 최솟값을 구한 후 최솟값으로 변환한다.
ds2 <- arrange(ds, -ds$medv)
ds2$medv[c(1:50)] <- min(ds2$medv[c(1:50)])
ds2$medv

ds2_crim <- ds2[ds2$crim > 1, ]
result <- mean(ds2_crim$crim)
print(result)
