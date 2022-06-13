library(svDialogs)
setwd('C:\\Rworks')

covid <- read.csv('부산광역시_사상구_코로나확진자발생비율.csv', header=T)
covid

year <- dlgInput('Input year')$res
year

my_year <- subset(covid, 년==year)

# 만약에 csv파일로 쓰라고 했을 경우에 적용할 코드
write.csv(my_year, 'year.csv', row.names=F)
new_my_year <- read.csv('year.csv')
class(new_my_year)
str(new_my_year)


# subset으로 해당 년도의 데이터만 추출합니다.
# 년 정보는 4번째에 해당함.
# covid의 구조를 파악

# 인구수열만 추출하고 특수문자를 제거
people <- gsub("[[:punct:]]", "", my_year$인구수)
# 계산을 위해서 숫자형으로 변경
people <- as.numeric(people)
people

result <- 0
for(i in 1:length(people)) {
  result = result + people[i]
}

cat(year,'년 총 확진자수는 =',result, '명 입니다')