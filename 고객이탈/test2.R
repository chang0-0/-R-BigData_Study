library(MASS)
library(mlbench)
library(dplyr)
data(Boston)
ds <- Boston

summary(ds)

top10 <- head(sort(ds$crim, decreasing = TRUE), 10)
tenth <- top10[10]

ds$crim <- ifelse(ds$crim >= tenth, tenth, ds$crim)

