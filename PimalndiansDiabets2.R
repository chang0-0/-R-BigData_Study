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
ds2 <- ds2 %>% mutate(age_grp = ifelse(age >= 60, "3", ifelse(age >= 41, "2", "1")))
head(ds2)
summary(ds2)

# 6. 발병률이 가장 높은 나이 그룹의 발병률을 구하시오 (발병률 = diabetes 중 pos의 수 / 인원 수)
sum_ds2 <- ds2 %>% group_by(age_grp) %>% summarise(total_num = n(), sum(diabetes == "pos"), ill_rate = diab_num/total_num) %>% arrange(desc(ill_rate))
result <- head(sum_ds2$ill_rate, 1)
print(result)


#---------------------------------------------------------------------------

library(mlbench)
data(PimaIndiansDiabetes2)
ds <- PimaIndiansDiabetes2

summary(ds)

# 결측값이 없는 데이터 세트로 만들기
# 결측치가 없는 행만 반환
ds2 <- ds[complete.cases(ds),]
print(ds2)

# 특정 컬럼의 결측값을 제외하고 가져오기
ds3 <- ds %>% filter(!is.na(glucose & pressure & mass))
colSums(is.na(ds3))

ds3 <- transform(ds3, age = ifelse(age >= 20 & age <= 40, 3, ifelse(age >= 41 & age <= 60, 2, ifelse(age >= 61, 1))))

# 나이를 그룹화하여 범주화 시킨 컬럼 새로추가
ds3 <- ds3 %>% mutate(age_grp = ifelse(age >= 60, "3", ifelse(age >= 41, "2","1")))
summary(ds3)

# 발병률이 가장 높은 나이 그룹의 발병률을 구하시오.
# mutate는 데이터를 덮어쓰거나 새로운 컬럼을 추가할때 사용하는 함수.
# group_by와 summarise는 tibble형태로 값을 반환함
sum_ds3 <- ds3 %>% group_by(age_grp) %>% summarise(total_num = n(), diab_num = sum(diabetes == "pos"), 
ill_rate = diab_num/total_num) %>% arrange(desc(ill_rate))

result <- max(sum_ds3$ill_rate)
print(result)
