 # 패키지 설치
install.packages("readr")
install.packages("dplyr")
library(readr)
data = read_csv('./Train.csv')
str(data)

set.seed(1)
idx = sample(1:nrow(data), nrow(data) * 0.7)
X_train = data[idx, -ncol(data)]; y_train = data[idx, ncol(data)]
print(X_train)

