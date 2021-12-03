list.files()

library(dplyr)

df <- read.csv("./Loan payments data.csv")

summary(df)
colSums(is.na(df))
