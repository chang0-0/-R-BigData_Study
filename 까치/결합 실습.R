# install.packages("quantmod")

# yahoo finance 웹사이트로 부터 주가데이터와 환율데이터를 다운로드함
# 종목코드와 환율코드를 입력하면 날짜별로 데이터를 가져올 수 있음
library(quantmod)

sec <- getSymbols(Symbols = "005930.KS", from = "2021-10-01", to = "2021-12-31", auto.assign=FALSE )
str(sec)

# str로 확인해보면 list형이기때문에
# dataframe형태로 수정했음
sec <- as.data.frame(sec)

head(sec[c("005930.KS.Close", "005930.KS.Volume")])

sec <- cbind(date = rownames(sec), 
symbol = "005930.KS", 
sec[c("005930.KS.Close", "005930.KS.Volume")])

head(sec)
rownames(sec) <- NULL
colnames(sec)[c(3, 4)] <- c("close", "volume")
head(sec)

# 현대자동차의 주가데이터와 삼성전자의 주가데이터셋을 합침

hmc <- getSymbols(Symbols = "005387.KS", from = "2021-10-01", to = "2021-12-31", auto.assign=FALSE ) 
hmc <- as.data.frame(hmc)
head(hmc)

head(hmc[c("005387.KS.Close", "005387.KS.Volume")])

hmc <- cbind(date = rownames(hmc), symbol = "005387.KS",
hmc[c("005387.KS.Close", "005387.KS.Volume")] )

rownames(hmc) <- NULL
colnames(hmc)[c(3, 4)] <- c("close", "volume")

head(hmc)
head(sec)

stock <- rbind(sec, hmc)
stock

# merge함수를 활용해서 열을 추가하기
fx <- getSymbols(Symbols = "KRW=X", from = "2021-10-01", to = "2021-12-31", auto.assign=FALSE ) 
fx <- as.data.frame(fx)
head(fx[c("KRW=X.Close")])


fx <- cbind(date = rownames(fx), fx[c("KRW=X.Close")])
head(fx)


rownames(fx) <- NULL
colnames(fx)[c(2)] <- c("close")
head(fx)


# sec와 fx를 공통의 열을 기준으로 결합

# 공통열이 뭐가 있는지 확인
intersect(names(sec), names(fx))

# 날짜를 기준으로해서 데이터를 결합
report <- merge(sec, fx, by = "date")
head(report)

v <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
# index값을 반환함.
# 7이 v의 4번째에 위치하므로 4가 반환이 됨
match(7, v)
match(c(11, 5, 3, 1, 0), v)
# 없는 값들은 NA가 출력됨

head(mtcars)
car <- mtcars
head(car)

car$name <- rownames(car)
rownames(car) <- NULL
head(car)

# 힘이 좋은 자동차 데이터셋 145hp 초과
highhp.car <- car[car$hp > 145,]


# 가벼운 자동차 모델의 서브데이터 셋
# 3200 파운드 미만의 자동차
lightwt.car <- car[car$wt < 3.2,]


head(highhp.car)
head(lightwt.car)


index <- match(highhp.car$name, lightwt.car$name)
index

lightwt.car[na.omit(index), ]


# %in% 연산자
# %in% 연산자 앞 뒤로 붙는 벡터가 서로 일치하는지의 여부를 논리값으로 반환함

c(7, 2, 1, 0) %in% v

index <- highhp.car$name %in% lightwt.car$name
index

# TRUE인 행만 출력
highhp.car[index,]
