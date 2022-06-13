setwd('C:\\Rworks')

library(ggplot2)
str(diamonds)

# 커팅 품질이 Premium이며 무게가 2캐럿 이상인 다이아몬드 데이터를 추출하여 변수 diamonds.new에 저장합니다.
diamonds.new <- subset(diamonds, cut == 'Premium' & carat >= 2)
diamonds.new

write.csv(diamonds.new, 'shiny_diamonds.csv', row.names=F)

diamonds.load <- read.csv('shiny_diamonds.csv', header=T)

# 색에 해당하는 열을 확인한 다음 다음 열 값이 D인 경우를 제외하여 변수 diamonds.new에 저장합니다.
diamonds.new <- subset(diamonds.load, color != 'D')

library(xlsx)
write.xlsx(diamonds.new,'shiny_diamonds.xlsx', row.names='F')