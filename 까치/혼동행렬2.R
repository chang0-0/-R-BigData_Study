library(mlbench)
data(Ionosphere)
df = Ionosphere

library(caret)
idx = caret::createDataPartition(df$Class, p = 0.7)

df_train = df[idx$Resample1, ] 
df_test = df[-idx$Resample1, ]

head(df_train)

#로지스틱 회귀 모델
# 로지스틱 회귀 에서는 family=binomial로 지정해주어야한다.
model_glm = glm(Class ~ . , data = subset(df_train, select = c(-V1, -V2)), family = "binomial")
model_glm = step(model_glm, direction = "both")

# install.packages("car")
library(car)
vif(model_glm)

pred_glm_test = predict(model_glm, newdata = df_test, type = "response")

head(pred_glm_test)

pred_glm_test_class = factor(ifelse(pred_glm_test >= 0.5, 1, 0))
levels(pred_glm_test_class) <- c("bad", "good")
head(pred_glm_test_class)

cfm_glm = caret::confusionMatrix(pred_glm_test_class, df_test$Class, positive="good")
cfm_glm

########################## 랜덤포레스트 ########################## 

library(randomForest)
model_rf = randomForest(Class ~ . , data = subset(df_train, select = c(-V1, -V2) ))
pred_rf_test = predict(model_rf, newdata = df_test, type='response')
cfm_rf = caret::confusionMatrix(pred_rf_test, df_test$Class)

head(pred_rf_test)

cfm_rf


















