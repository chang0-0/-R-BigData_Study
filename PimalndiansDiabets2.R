# 1. 패키지 설치 및 라이브러리 호출
install.packages("mlbench")
library(mlbench)
library(dplyr)

# 2. 데이터 호출 및 변수 지정
data("PimaIndiansDiabetes2")
ds <- PimaIndiansDiabetes2

# 3. 데이터 통계량 확인
summary(ds)

# 아래의 코드 경우 전체 NA결측값 제거 됨
# ds2 <- na.omit(ds)
# colSums(is.na(ds2))

# 4. glucose, pressure, mass 컬럼의 결측값 있는 행 제거
# 특정컬럼이 결측치인 행 제거 
# (https://zetawiki.com/wiki/R_%ED%8A%B9%EC%A0%95%EC%BB%AC%EB%9F%BC%EC%9D%B4_%EA%B2%B0%EC%B8%A1%EC%B9%98%EC%9D%B8_%ED%96%89_%EC%A0%9C%EA%B1%B0)
ds2 <- ds %>% filter(!is.na(glucose) & !is.na(pressure) & !is.na(mass))
colSums(is.na(ds2))

# 5. 나이(age)를 조건에 맞게 그룹화
# (1: 20~40세, 2: 41~60세, 3: 60세이상) 
ds2 <- ds2 %>% mutate(
    if(age_grp >= 20 && age_grp <= 40 ) {
        "1"
    } elseif(age_grp >= 41 %% age_grp <= 59){
        "2"
    } elseif(age_grp >= 60) {
        "3"
    }

)

# 발병률이 가장 높은 나이 그룹의 발병률을 구하시오 (발병률 = diabetes 중 pos의 수 / 인원 수)

