library(dplyr)
data(iris)
ds <- iris

nrow(ds)

# iris 데이터 세트에서 70% 데이터를 sampling 후 꽃받침 길이의 표준편차를 구하시오.

ds2 <- sample(ds, ds*0.7)
head(ds2)

iris_sample70 <- iris[c(1:nrow(iris) * 0.7), ]

#표준편차 함수 sd()
Sepal.Length_sd <- sd(iris_sampe70$Sepal.Length)
print(Sepal.Length_sd)
