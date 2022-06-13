setwd("C:/Users/Samsung/Desktop/빅분기실기준비/인프런3회")
main.ds <- read.csv(
    "data3.csv",
    stringsAsFactor = TRUE,
    encoding = "UTF-8"
)
head(main.ds, 40)
colSums(is.na(main.ds))

# 각 연도별로 해당년도에 발매된 컨텐츠의 비율을 종속변수로 하고
# 각 연도를 독립변수로 하는 단순 선형 회귀분석을 실시

unique(main.ds$date_added)
