library(dplyr)
library(ModelMetrics)
library(tidyr)
library(caret)

data(mtcars)

set.seed(101)
parts <- createDataPartition(
    mtcars$mpg,
    p = 0.7,
    list = FALSE
)

str(mtcars)

train <- mtcars[parts, ]
test <- mtcars[-parts, ]

pro <- preProcess(
    train[, -1],
    method = c("range")
)

sc_train <- predict(pro, train)
sc_test <- predict(pro, test)



model <- lm(mpg ~ ., sc_train)
summary(model)

step(model, direction = 'both')

form <- formula(mpg ~ wt + qsec)
model <- lm(form, sc_train)

pred <- predict(
    model,
    newdata = sc_test,
    type = 'response'
)

pred
mtcars$mpg

rmse(test$mpg, pred)


main <- mtcars
main$count <- 1
str(main)

main[main$gear == 3, ]
main[main$gear == 4, ]
main[main$gear == 5, ]


agg <- aggregate(
    count ~ gear
    main,
    FUN = sum
)
agg
