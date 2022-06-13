install.packages("plyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("MASS")

library(plyr)
library(dplyr)
library(magrittr)
library(MASS)

str(iris)

iris$Species[1:2] <- "A"
head(iris)

# factor로 구성된 데이터에 ㅅ로운 factor 추가/변경
# 이전에 Factor요소였던 column이 character형으로 변경됨.
iris$Species = as.character(iris$Species)
iris$Species[1:2] <- "A"
str(iris)
head(iris)

iris$Species = as.factor(iris$Species)
head(iris)

list.files()
df = read.csv("example.csv")
df
str(df)

df$list = as.factor(df$list)
df$interview = as.factor((df$interview))

levels(df$interview)
df$interview = factor(df$interview, levels = c("Very bad", "Bad", "Normal", "Good", "Very good"))
levels(df$interview)

#interview_score 컬럼을 추가해서 interview 등급을 점수로 매긴다.
# 평균을 낼 수 있음

df[, "interview_score"] = as.numeric(df$interview)
mean(df$interview_score)

# data frame에서 column만 남기고 내용 모두 지우기
df = df[, c("list", "interview", "interview_score")]
# 기존 데이터에서 구조는 동일하게 값만 새로 입력하고 싶은 경우,
# 기존의 데이터와 구조는 같지만 같은 값은 없도록 하고 싶은 경우

process_data = df[0, 1] 

# data frame에 row 추가하기

df[(nrow(df) + 1), ] = c("A1", "Good", 4)

# 원하는 컬럼만 선택 / 제거하기

head(mtcars)

# 1~3 col만 선택
mtcars_1_3_col = mtcars[, c(1:3)]
head(mtcars_1_3_col, 1)

# 위와 반대인 1~3 col만 빼고 나머지 선택
mtcars_3_1_exc_col = mtcars[, -c(1:3)]
mtcars_3_1_exc_col

mtcars_1_3_col_name = mtcars[,c("mpg", "cyl", "disp")]
head(mtcars_1_3_col_name)

#  NA제거하기

# mtcars에서 mpg가 NA인 데이터만 50으로 변경함
is.na(mtcars$mpg)
mtcars$mpg[1:2] = 50
mtcars

mtcars[is.na(mtcars$mpg), "mpg"] = 50

install.packages("mtcars")
library(mtcars)
data(mtcars)
df = mtcars

df$mpg[1:2] = "A"
head(df)

