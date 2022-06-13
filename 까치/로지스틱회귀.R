# install.packages("ISLR")
library(ISLR)
data(Default)
df = Default

# replae = FALSE (비복원추출)
set.seed(20)
train_idx = sample(1:nrow(df), size=0.8*nrow(df), replace=FALSE)
test_idx = (-train_idx)

df_train = df[train_idx, ]
df_test = df[test_idx, ]
dim(df_train)
dim(df_test)

str(Default)

idx2 = caret::createDataPartition(Default$default, p = .8)

df_test2 = df[idx2$Resample1, ]
df_train2 = df[-idx2$Resample1, ]

dim(df_test2)
dim(df_train2)


df_glm = glm(default ~ . , family = binomial, data = df_train)

step_model = step(df_glm, direction="both")
summary(step_model)

null_deviance = 2333.8
residual_deviance = 1258.0
model_deviance = null_deviance - residual_deviance

# df를 2로 넣어주는 이유는 Null deviance의 자유도가 7999이고, Residual의 자유도가 7997이므로
# 둘의 자유도가 2차이 나기 때문에 df=2 입니다.
pchisq(model_deviance, df=2, lower.tail = FALSE)

vif(step_model)
pred = predict(step_model, newdata = df_test[, -1], type="response")
df_pred <- as.data.frame(pred)

# 확률 0.5를 기준으로 파산 여부 변환 (Yes/No)

df_pred$default <- as.factor(ifelse(df_pred$pred >= 0.5, df_pred$default <- "Yes", df_pred$default <- "No"))
de <- df_pred$default

fitted(df_glm)


caret::confusionMatrix(data =df_pred$default, reference = df_test[,1])

library(ModelMetrics)
auc_model <- auc(actual = df_test[, 1], predict = df_pred$default)
# auc는 불량의 성능값을 나타냄












