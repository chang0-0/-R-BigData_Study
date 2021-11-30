# 2번째 브랜드 별 햄버거 영양성분 비교
Kcal <- c(514, 533, 566, 506)
na <- c(917, 853, 888, 667)
fat <- c(11, 13, 10, 6)
menu <- c('새우', '불고기', '치킨', '싸이')

burger<- data.frame(kcal, na, fat, menu)
str(burger)
burger

rownames(burger) <- c('M','L','B','Mo')

burger['Mo', 'na']
burger['Mo',]
burger[, 'kcal']
burger[c("Mo",'B'),'menu']
