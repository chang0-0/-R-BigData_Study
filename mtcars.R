library(dplyr)

data(mtcars)
ds <- mtcars

summary(ds)

cnt4 <- ds %>% filter(ds$cyl == 4)
cnt4 <- nrow(cnt4)
cnt4

total <- length(ds$cyl)
total

a <- cnt4/total
print(a)
print(a[1])
