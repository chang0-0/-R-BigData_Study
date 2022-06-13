data(airquality)
df <- airquality
head(df)

mean(df$Temp)

# AVG = 76
# 단일 표본 T검정
result = t.test(x = df$Temp, mu = 76.5)


result = t.test(x = df$Temp, mu = 76)
result

result$p.value
result$statistic

str(result)

names(result)

print(result[[1]])
print(result[["statistic"]])
print(result[["p.value"]])

head(iris, 2)

# 대응표본 t-검정
t.test(iris$Sepal.Length, iris$Sepal.Width, paired = TRUE)
mean(iris$Sepal.Length) - mean(iris$Sepal.Width)

# 독립표본 t-검정
str(iris$Sepal.Length, iris$Sepal.Width)

# 등분산 검정
bartlett.test(iris$Sepal.Length ~ iris$Sepal.Width, data = iris)  

aggregate(data = iris, Sepal.Length ~ Sepal.Width, FUN = "var")
aggregate(data = iris, Sepal.Width ~ Sepal.Length, FUN = "var")

 # 등분산 가정
t.test(iris$Sepal.Length, iris$Sepal.Width, paired = FALSE, val.equal = TRUE)
var.test(iris$Sepal.Length, iris$Sepal.Width)

str(iris$Sepal.Length)
str(iris$Sepal.Width)

# 등분산 검정
# F-검정, 등등..
