library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)
library(tidyr)
library(ggplot2)

x_train  <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/HRdata/X_train.csv", 
    stringsAsFactor = TRUE, 
    na.strings = c("", "na", "NA", NA))
y_train  <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/HRdata/y_train.csv", 
    na.strings = c("", "na", "NA", NA))
x_test <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/HRdata/X_test.csv", 
    stringsAsFactor = TRUE, 
    na.strings = c("", "na", "NA", NA))


nrow(x_train)
nrow(y_train)
nrow(x_test)

summary(x_train)
summary(y_train)


# target 컬럼 예측하기
full <- merge(x_train, y_train, "enrollee_id")
full <- full[, -c(1)]



# full 결측값 제거
colSums(is.na(full))
full <- full %>% fill(education_level, .direction = 'updown')
full <- full %>% fill(company_type, .direction = 'updown')
full <- full %>% fill(company_size, .direction = 'updown')
full <- full %>% fill(major_discipline, .direction = 'updown')
full <- full %>% fill(last_new_job, .direction = 'updown')
full <- full %>% fill(enrolled_university, .direction = 'updown')
full <- full %>% fill(experience, .direction = 'updown')

full$gender[is.na(full$gender)] <- 'Male'
full$gender[full$gender == 'Other'] <- 'Male'
full$gender <- factor(full$gender, levels = c('Male', 'Female'))
table(full$gender)

full$relevent_experience <- ifelse(full$relevent_experience == 'Has relevent experience', 'Yes', 'No')
full$relevent_experience <- as.factor(full$relevent_experience)
full$relevent_experience <- relevel(full$relevent_experience, 'Yes') 

full$target <- ifelse(full$target == 1, 'Yes', 'No')
full$target <- as.factor(full$target)
full$target <- relevel(full$target, 'Yes') 


full$city <- as.character(full$city)
colSums(is.na(full))


################################################################################################


x_test <- x_test %>% fill(education_level, .direction = 'updown')
x_test <- x_test %>% fill(company_type, .direction = 'updown')
x_test <- x_test %>% fill(company_size, .direction = 'updown')
x_test <- x_test %>% fill(major_discipline, .direction = 'updown')
x_test <- x_test %>% fill(last_new_job, .direction = 'updown')
x_test <- x_test %>% fill(enrolled_university, .direction = 'updown')
x_test <- x_test %>% fill(experience, .direction = 'updown')


x_test$gender[is.na(x_test$gender)] <- 'Male'
x_test$gender[x_test$gender == 'Other'] <- 'Male'
x_test$gender <- factor(x_test$gender, levels = c('Male', 'Female'))
table(x_test$gender)

x_test$relevent_experience <- ifelse(x_test$relevent_experience == 'Has relevent experience', 'Yes', 'No')
x_test$relevent_experience <- as.factor(x_test$relevent_experience)
x_test$relevent_experience <- relevel(x_test$relevent_experience, 'Yes') 
x_test$city <- as.character(x_test$city)

colSums(is.na(x_test))

str(full)
str(x_test)

colSums(is.na(full))
colSums(is.na(x_test))

rf <- randomForest(
    target ~ . ,
    full,
    ntree = 300,
    do.trace = TRUE
)

auc(rf)

pred <- predict(
    rf, 
    newdata = x_test,
)

head(pred, 10)

list <- ifelse(pred == 'Yes', 1, 0)
list <- as.factor(list)
list

result <- data.frame(
    x_test$enrollee_id,
    list
)

head(result)

names(result) <- c("enrollee_id", 'target')
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/데이터마님 빅분기/2유형 연습1/이직여부 예측")
write.csv(result, "result.csv", row.names = F)


Rtest <- read.csv('result.csv')
head(Rtest)
