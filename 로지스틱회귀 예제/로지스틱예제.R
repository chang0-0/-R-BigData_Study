library(mlbench)
data("BreastCancer")

ds <- BreastCancer
head(ds)
str(ds)
summary(ds)
nrow(ds)

table(ds$Class)
# 양성(benign) 음성(malignant) 2가지로 분류 가능

#결측값 확인
colSums(is.na(ds))
sapply(ds, FUN = function(x) {
    sum(is.na(x))
})

#결측값 제거
ds <- ds[complete.cases(ds), ]
nrow(ds)

# 중복데이터확인하기
sum(duplicated(ds))

ds <- ds[!duplicated(ds), ]
nrow(ds)

table(ds$Class)

cat("   total : " , margin.table(table(ds$Class)))
prop.table(table(ds$Class))

# 독립변수간에 다중공선성을 확인
Y <- ifelse(ds$Class == 'malignant', 1, 0) 
X <- ds[, c(2:10)]
str(X)

# integer형으로 전체 변환
for(n_col in 1:9) {
    X[, n_col] <- as.integer(X[, n_col])
}


install.packages("fmsb")
library(PerformanceAnalytics)
windows(height = 8, width = 8)
chart.Correlation(X, histogram = TRUE, col = "grey10", pch = 1)

library(GGally)
ggcorr(X, name = "corr", label = T)

library(fmsb)
str(X)

X2 <- scale(X)
var(X2[, ])

library(dplyr)
ds <- data.frame(Y, X2)
X_names <- names(data.frame(X2))
t.test_p.value_df <- data.frame()

for(i in 1:length(X_names)) {
    t.test_p.value <- t.test(ds[, X_names[i]] ~ ds$Y, var.equal = TRUE)$p.value
    t.test_p.value_df[i, 1] <- X_names[i]
    t.test_p.value_df[i, 2] <- t.test_p.value
}

colnames(t.test_p.value_df) <- c("x_var_name", "p.value" )
t.test_p.value_df <- t.test_p.value_df %>% arrange(p.value)
t.test_p.value_df


# 데이터셋 80 : 20으로 분리

set.seed(122)
train <- sample( 1:nrow(ds), size = nrow(ds) * 0.8 , replace = FALSE)
test <- (-train)

Y.test <- Y[parts]
scales::percent( length(train) / nrow(ds)  )

glm.fit <- glm(Y ~ . , data = ds, 
    family = binomial(link = "logit"), subset = train
)
summary(glm.fit)
formula(glm.fit)


step(glm.fit, direction = "backward")
summary(glm.fit)

glm.fit2 <- glm(Y ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin +
    Normal.nucleoli + Mitoses, 
    data = ds, family = binomial(link = "logit"), subset = train)


anova(glm.fit2, test = "Chisq")

glm.probs <- predict(glm.fit2, ds[test, ] , type = "response" )
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
glm.pred

# factor형으로 변환
glm.pred <- factor(
    ifelse(glm.probs > 0.5, 1, 0), 
        levels = c(0, 1),
        labels = c(0, 1)
)


table(Y.test, glm.pred)

Y.test <- as.factor(Y.test)
length(Y.test)
length(glm.pred)

mean( Y.test == glm.pred)
mean(Y.test != glm.pred)
