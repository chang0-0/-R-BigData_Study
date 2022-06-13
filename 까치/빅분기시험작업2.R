options(scipen = 1000)

library(dplyr); library(caret); library(randomForest); library(pROC)

# 데이터 불러오기

X_test = read.csv('data/X_test.csv') 
X_train = read.csv('data/X_train.csv') 
y_train = read.csv('data/y_train.csv')

# 데이터 확인

#str(X_train) ; str(X_test) ; str(y_train) #X_train, X_test 주구매상품 42,41로 다름 

#ifelse(unique(X_train$주구매상품) %in% unique(X_test$주구매상품), 1, 0) #41번째 없음

#unique(X_train$주구매상품) #소형가전 제품이 X_test에는 존재 X

#X_train %>% filter(X_train$주구매상품 == '소형가전') #2가지 경우라서 나중에 삭제하기

​

#summary(X_train) ; summary(X_test) #총구매액, 최대구매액 음수값존재 환불금액 NA값존재

#colSums(is.na(X_train)) #환불금액 2295개

#colSums(is.na(X_test)) #환불금액 1611개 

​

# train 데이터 조인 및 전처리

full <- left_join(y_train, X_train, by = 'cust_id')

full <- full %>% dplyr::select(-cust_id) #cust_id제거 select함수 여기 환경에서 안될때 있으니 dplyr:: 사용 

#str(full) 

full$gender <- as.factor(full$gender) #gender 범주화

#full %>% filter(full$총구매액 == 0) # 총구매액이 0인 경우는 최대구매액 < 환불금액인 경우인듯 

#full %>% filter(full$최대구매액 == 0) #최대구매액이 0인 경우는 X

#full %>% filter(full$총구매액 < 0) #경우 3개

#full %>% filter(full$최대구매액 < 0) #경우 1개 총구매액이랑 최대구매액이 -2992000으로 같고 환불금액도 2992000임 

​

#음수값들 평균대치하기

full$총구매액 <- ifelse(full$총구매액 < 0 , mean(full$총구매액), full$총구매액) 

full$최대구매액 <- ifelse(full$최대구매액 < 0 , mean(full$최대구매액), full$최대구매액)

full$환불금액 <- ifelse(is.na(full$환불금액), 0, full$환불금액)

#str(full)

##############################Train

​

#원핫인코딩 

gender_full <- full$gender

dummy <- dummyVars('~.-gender',full)

full <- data.frame(predict(dummy,full))

full$gender <- gender_full

#str(full_norm) #소형가전 23번째 컬럼 삭제

full <- full[,-23]

​

##############################Test

custid <- X_test$cust_id #나중을 위해 빼놓기 

X_test <- X_test %>% dplyr::select(-cust_id)

X_test$환불금액 <- ifelse(is.na(X_test$환불금액), 0, X_test$환불금액)

X_test$총구매액 <- ifelse(X_test$총구매액 < 0 , mean(X_test$총구매액), X_test$총구매액)

X_test$최대구매액 <- ifelse(X_test$최대구매액 < 0 , mean(X_test$최대구매액), X_test$최대구매액)

​

#원핫인코딩

dummy <- dummyVars('~.',X_test)

X_test <- data.frame(predict(dummy,X_test))

​

#데이터 분할 

idx <- createDataPartition(full$gender, p = 0.7, list = FALSE)

train <- full[idx,]

valid <- full[-idx,]

​

#randomForest

model <- randomForest(gender~. , train)

pred_valid <- predict(model, valid, type = 'prob')

auc(valid$gender, pred_valid[,2]) #0.6567

​

#정답제출하기

pred_test <- predict(model, X_test, type = 'prob')

​

answer <- data.frame(custid = custid, gender = pred_test[,2])

answer %>% head(10)
