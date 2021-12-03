library(dplyr)

setwd("C://Users//Samsung//Desktop//빅분기 실기 준비//FIFA_Ranking")
list.files()

datas <- read.csv("fifa_ranking.csv", header=T,  fileEncoding = "UTF-8-BOM")

# total_point datas에서 total_point 선택후 내림차순으로 정렬
datas_point <- datas %>% select(total_points) %>% arrange(desc(total_points))
rank3 <- datas_point[3, ]
print(rank3)

datas %>% filter(datas_point >= rank3) %>% select(country_abrv)
v_mean <- datas %>% filter(country_abrv %in% c('GER', 'ITA', 'SUI')) %>%
    summarise(mean = mean(total_points, na.rm=TRUE))
print(v_mean)
