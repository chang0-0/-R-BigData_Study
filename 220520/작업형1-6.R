library(reshape2)
library(dplyr)
list.files()

ds2 <- read.csv("TB_notifications.csv")
ds2 <- ds2[, c("year", "country", "new_sp")]
head(ds2)
summary(ds2)
ds2 <- na.omit(ds2)

ds2.dcast <- dcast(ds2, year ~ country, value.var = "new_sp")

# 2000년도 데이터만 추ㅜㄹ
ds2.2000 <- ds2.dcast %>% filter(year == 2000)

# year 변수 제거
ds2.2000 <- ds2.2000[, -1]

str(ds2.2000)

trans_dt <- melt(ds2.2000, variable.name = "rt", value.name = "country")
mean_country <- mean(trans_dt$rt, na.rm = TRUE)
