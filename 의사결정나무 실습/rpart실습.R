library(caret)
library(rpart)

data(iris)
main.ds <- iris
head(main.ds)
parts <- caret::createDataPartition(main.ds$Species, p = 0.8)

train <- main.ds[parts$Resample1, ]
test <- main.ds[-parts$Resample1, ]

dt.m <- rpart(
    Species ~ .,
    data = train
)

dt.m
windows(height = 8, width = 8)
plot(
    dt.m, 
    compress = TRUE,
    margin = 0.3
)
text(dt.m, cex = 1.5)


library(rpart.plot)
prp(
    dt.m,
    type = 2,
    extra = 104,
    fallen.leaves = TRUE,
    roundint = TRUE
)


dt.m.pred <- predict(
    dt.m,
    newdata = test,
    type = 'class'
)

dt.m.pred
caret::confusionMatrix(
    test$Species,
    dt.m.pred
)

library(rpart.plot)


mean(test$Species == dt.m.pred)

prp(dt.m, type = 4, extra = 2)

