# 참고: https://www.kaggle.com/code/evgenigeorg/predict-the-sales-price-with-mlr3-in-r#importing-packages

install.packages("tidyverse")
library(tidyverse)
library(mlr3verse)
library(skimr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

sapply(train, FUN = function(x) {
        sum(is.na(x))
})

sapply(test, FUN = function(x) {
        sum(is.na(x))
})


# skim함수는 기술통계분석에 사용된다.
skim(train)
skim(test)

character2factor = function(x) {
        x %>% mutate_if(sapply(x, is.character), as.factor)
}

train = character2factor(train)
test = character2factor(test)



impute_naive = function(x, draw=FALSE) {
  x = as.data.frame(x)
  
  cols_imputed = lapply(x, function(col) {
    which_na = which(is.na(col))
    num_na = length(which_na)
    
    if (is.numeric(col)) {
      if (!draw) {
        col = replace(col, is.na(col), median(col, na.rm = TRUE))
      } else {
        col_range = range(col, na.rm = TRUE)
        vals_imputed = runif(n = num_na, min = col_range[1], max = col_range[2])
        col = replace(col, which_na, vals_imputed)
      }
    } else {
      col_levels = levels(col)
      if (!draw) {
        col_mode = col_levels[which.max(table(col))]
        col = replace(col, is.na(col), col_mode)
      } else {
        col_proportions = prop.table(table(col))
        vals_imputed = sample(length(col_levels), num_na, prob = col_proportions, 
                              replace = TRUE)
        col = replace(col, which_na, col_levels[vals_imputed])
      }
    }
    
    col
  })
  
  do.call(cbind.data.frame, cols_imputed)
}

train = as_tibble(impute_naive(train))
test = as_tibble(impute_naive(test))

# create the classification task
task = TaskRegr$new(id = "HousePrices", backend = train, target = "SalePrice")

# creates a learner

learner = lrn("regr.ranger")

# train learner on the task
learner$train(task)

# predict
pred = learner$predict_newdata(test)

# create the classification task
task = TaskRegr$new(id = "HousePrices", backend = train, target = "SalePrice")

# creates a learner
learner_gb = lrn("regr.xgboost", nrounds = 100)

# Construct Operators
fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
graph = fencoder %>>% learner_gb
graph_learner = as_learner(graph)

# train learner on the task
graph_learner$train(task)

pred_gb <- graph_learner$predict_newdata(test)
pred_gb

# create the classification task
task = TaskRegr$new(id = "HousePrices", backend = train, target = "SalePrice")

# creates a learner
learner_glmnet = lrn("regr.cv_glmnet")

# Construct Operators
fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
graph_glmnet = fencoder %>>% learner_glmnet
graph_learner_glmnet = as_learner(graph_glmnet)

# train learner on the task
graph_learner_glmnet$train(task)

# predict
pred_glmnet = graph_learner_glmnet$predict_newdata(test)

# create a dataframe with our results
my_submission = tibble('Id' = test$Id, 'SalePrice' = pred_gb$response)

# save our file
write_csv(my_submission, 'submission.csv')
