library(svDialogs)
setwd('C:\\Rworks')

covid <- read.csv('부산광역시_사상구_코로나확진자발생비율.csv', header=T)
covid

year <- dlgInput('Input year')$res
year

my_year <- subset(covid, 년==year)
my_year

my_year <- gsub("[[:punct:]]", "", my_year$인구수)
my_year <- as.numeric(gsub(',',"",my_year))
my_year
sum(my_year)

# 함수로 만들어서 콤마 없애버리고 숫자형으로 변환함
setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )
DF <- read.csv('부산광역시_사상구_코로나확진자발생비율.csv',colClasses=c('num.with.commas','factor','character','numeric','num.with.commas'))
DF
str(DF)

# Ư?????? ��??
# people <- gsub("[[:punct:]]", "", my_year$?α???)
# ????�� ��?ؼ? ??????��?? ????
# people <- as.numeric(people)








