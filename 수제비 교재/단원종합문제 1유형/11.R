library(dplyr)
library(lubridate)

data(airquality)

# 8월 20일의 Ozone 값을 구하시오

str(airquality)
head(airquality, 10)

airquality$Day
str(airquality)

result11 <- airquality %>% filter(Month == 8 & Day == 20) %>% select(Ozone)
print(result11)


####################################################################################

a <- ymd("960608")
b <- as.Date("1996-06-08")
class(b)
str(b)
year(b)

year(a)

month(a)
day(a)

week(a)

# wday는 기본적으로 일요일 ~ 토요일 1 ~ 7로 표현
wday(a)
# 토요일

wday(a, label = TRUE)
wday(a, label = TRUE)
yday(a)
semester(a)

quarter(a)

a+9500

today() - a
t <- abs(today() - a)
t
