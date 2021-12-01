library(dplyr)

# 8월 21일의 Ozone 값

data(airquality)
ds <- airquality

summary(ds)

ds2 <- ds %>% filter(Month == 8 & Day == 20) %>% select(Ozone)
print(ds2)
