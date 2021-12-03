library(dplyr)

data(esoph)
df <- esoph

summary(df)

# 환자 수와 대조군 수를 합한 새로운 컬럼인 관측자 수를 생성
df$nsums <- df$ncases + df$ncontrols
tail(df)

nsums_tab <- xtabs(nsums ~ alcgp + tobgp, data = df)
print(nsums_tab)