install.packages("faraway")
library(faraway)
library(dplyr)
data("orings")
ds <- orings
summary(ds)

str(ds)

ds2 <- ds %>% filter(damage >= 1)

result <- cor(ds2$temp, ds2$damage, method = "pearson")
print(result)

