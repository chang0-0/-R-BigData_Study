library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사3회")
main.ds <- read.csv(
    file = "insurance.csv",
    encoding = 'UTF-8',
    stringsAsFactor = TRUE
)

min(main.ds$charges)

summary(main.ds)
main.ds1 <- main.ds %>% select(-c(charges))
main.ds2 <- main.ds %>% select(charges)

set.seed(2021) 
parts <- sample(
    c(1:nrow(main.ds)),
    size = nrow(main.ds) * 0.7
)


train <- main.ds[parts, ]
test <- main.ds[-parts, ]

train$ID <- row.names(train)
test$ID <- row.names(test)


model_scale <- preProcess(
    train[, -8],
    method = c('range')
)

set.seed(2021) 
parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]

set.seed(2021)
md_rf <- randomForest(
    charges ~ . ,
    data = t.train,
    ntree = 500,
    do.trace = TRUE
)


md_rf.pred <- predict(
    object = md_rf,
    newdata = t.valid,
)

md_rf.pred
rmse(md_rf.pred, t.valid$charges)
# > rmse(md_rf.pred, t.valid$charges)
# [1] 4791.026


train.lm <- lm(
    t.train,
    formula = charges ~ . ,
)

step(train.lm , direction = 'both' )

form <- formula(charges ~ age + sex + bmi + children + smoker +
    region)


train.lm <- lm(
    t.train,
    formula = form
)


train.lm.pred <- predict(
    train.lm,
    newdata = t.valid,
)

rmse(train.lm, t.valid$charges)
# > rmse(train.lm, t.valid$charges)
# [1] 5639.978


final_pred <- predict(
    train.lm,
    newdata = test
)

final_pred

result <- data.frame(
    test$ID,
    final_pred
)

result
names(result) <- c("ID", "charges")
head(result)

mean(result$charges)

mean(result$charges)

test.result <- read.csv("result.csv")
test.result

min(result$charges)
min(test.result$charges)


x_train <- main.ds1[parts, ]
x_test <- main.ds1[-parts, ]

y_train <- main.ds2[parts, ]
y_test <- main.ds2[-parts, ]

y_train <- as.data.frame(y_train)
y_test <- as.data.frame(y_test)



model_scale <- preProcess(
    x_train[, -1],
    method = c('range')
)

model_scale

x_sc_train <- predict(
    model_scale,
    x_train
)

x_sc_test <- predict(
    model_scale,
    x_test
)

set.seed(2021) 
parts <- sample(
    c(1:nrow(x_train)),
    size = nrow(x_train) * 0.7
)

x_t <- x_sc_train[parts, ]
x_v <- x_sc_train[-parts, ]

y_t <- y_train[parts, ]
y_v <- y_train[-parts, ]


rf <- randomForest(
    charges ~ .,
    main.ds
)

pred <- predict(
    rfk
)
