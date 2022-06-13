library(dplyr)
lvs <- c("normal", "abnormal")

#lvs의 normal 86번 반복, abnormal 258번 반복
truth <- factor(
    rep(
        lvs, 
        times = c(86, 258)
    ),
    levels = rev(lvs)
)
head(truth)

pred <- factor(
    c(
        rep(lvs, times = c(54, 32)),
        rep(lvs, times = c(27, 231))
    ),
        levels = rev(lvs)
)

length(truth)
length(pred)
str(truth)

levels(truth)
levels(pred)

xtab <- table(pred, truth)
# 이원테이블
xtab

confusionMatrix(xtab)
confusionMatrix(xtab, positive = 'normal')


       