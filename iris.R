library(dplyr)
data(iris)
ds <- iris

mean_len <- mean(ds$Sepal.Length)
mean_wid <- mean(ds$Sepal.Width)

print(mean_len + mean_wid)
