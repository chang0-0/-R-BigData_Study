library(mlbench)
data(BostonHousing)
data <- BostonHousing

# boston데이터 상위 10개 항목
top10 <- head(sort(data$crim, decreasing = TRUE), 10)
tenth <- top10[10]

library(dplyr)
over80 <- data %>% filter(age >= 80)

summary(over80$age)

mean(over80$crim)
result <- mean(over80$crim)
print(result)


# 2번 
idx <- sample( x = c("train", "test"), size=nrow(Boston), replace = TRUE, prob = c(0.8, 0.2))

ds_train <- Boston[idx == "train", ]
ds_test <- Boston[idx == "test", ]

median_train <- median(ds_train$tax)
org_sd <- sd(ds_train$tax)

library(Train)
data("Train")

md <- lm(Reached.on.Time_Y.N~., train) 
step(lm (Reached.on.Time_Y.N~., train), direction = "both")


library("dplyr")
data(diamonds)
ds <- diamonds
str(ds)

nr0w