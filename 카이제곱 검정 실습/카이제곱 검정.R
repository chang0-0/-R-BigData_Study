survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol = 2)

# 카이제곱 검정을 위해서 교차표를 생성
dimnames(survivors) <- list(Status = c("minor injury", "minor injury", "dead"), 
    Seatbelt = c("With seatbelt", "Without seabelt"))

survivors

# 교차표에서 총합을 추가
addmargins(survivors)

# 열에만 sum 추가
addmargins(survivors, 2)

# 교차표를 비율로 생성
prop.table(addmargins(survivors, 2), 2)

# 합계가 100이되는 행 추가
addmargins(prop.table(addmargins(survivors, 2), 2), 1)


 