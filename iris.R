library(dplyr)
data(iris)
ds <- iris

ds_length <- ds %>% select(ds$Sepal.Length)
print(ds_length)
