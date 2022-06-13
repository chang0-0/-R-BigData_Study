library( dplyr )
library(scales)
library(caret)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사3회")
list.files()

main.ds <- read.csv(
    file = "insurance.csv",
    encoding = 'UTF-8',
    stringsAsFactor = TRUE,
)
nrow(main.ds)

set.seed(2021)
parts <- sample(
    c(1:nrow(main.ds)),
    size = nrow(main.ds) * 0.8
)

X_train <- main.ds[parts, ][-7]
X_test <- main.ds[-parts, ][-7]


y_train <- main.ds[parts, ][7]
y_test <- main.ds[-parts, ][7]


X_train$ID <- row.names(X_train)
X_train <- X_train %>% relocate(ID)

X_test$ID <- row.names(X_test)
X_test <- X_test %>% relocate(ID)

y_train$ID <- row.names(y_train)
y_train <- y_train %>% relocate(ID)

y_test$ID <- row.names(y_test)
y_test <- y_test %>% relocate(ID)

model_scale <- preProcess(
    X_train[, -1],
    method = c('range')
)


model_scale
scaled_X_train <- predict(model_scale, X_train)
scaled_X_test <- predict(model_scale, X_test)

set.seed(2021)
parts <- sample(
    c(1:nrow(X_train)),
    size = nrow(X_train) * 0.8
)


X_t <- scaled_X_train[parts, ]
X_v <- scaled_X_train[-parts, ]
y_t <- y_train[parts, ]
y_v <- y_train[-parts, ]

trControl <- trainControl(
    method = 'cv'
)

set.seed(2021)
model_glm <- train(
    x = X_t[, c(2:6)],
    y = y_t$charges, 
    method = 'glm',
    trControl = trControl
)

set.seed(2021)
model_rf <- train(
    x = X_t[, c(2:6)],
    y = y_t$charges, 
    method = 'rf',
    trControl = trControl,
    verbose = FALSE
)

getTrainPerf(model_glm)
getTrainPerf(model_rf)

set.seed(2021)
predict_glm <- predict(
    model_glm, 
    newdata = X_v[, c(2:6)]
)

predict_rf <- predict(
    model_rf,
    newdata = X_v[,c(2:6)]
)

valid_perf_glm <- RMSE(y_v$charges, predict_glm)
valid_perf_glm

valid_perf_rf <- RMSE(y_v$charges, predict_rf)
valid_perf_rf

test_rf <- predict(
    model_rf,
    newdata = X_test
)

result <- data.frame(
    X_test$ID, 
    test_rf
)

head(result)
names(result) <- c("ID", "charges")
write.csv(result, "result.csv", row.names = FALSE)
