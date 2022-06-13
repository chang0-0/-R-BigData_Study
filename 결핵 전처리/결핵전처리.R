setwd("C:/Users/Samsung/Desktop/빅분기실기준비/결핵 전처리")
list.files()
ds <- read.csv("TB_notifications.csv")

head(ds)
ds
str(ds)

ds <- ds[, c("year", "country", "new_sp")]
ds <- na.omit(ds)
head(ds)


#2000년도 국가의 평균 결핵 발생건수
wide.ds <- dcast(ds, year ~ country , value.var = "new_sp")

ds_2000 <- wide.ds %>% filter(year == 2000)
ds_2000 <- ds_2000[, -1]
head(ds_2000)
str(ds_2000)
