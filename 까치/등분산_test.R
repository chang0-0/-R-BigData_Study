library(dplyr)

data(iris)
df <- iris

data <- data.frame(species = c(1:100), sepal = c(1:100))

for(i in 1:100) {
    data$species[i] <- df$Species[i]
    data$sepal[i] <- df$Sepal.Length[i]
}

View(data)

boxplot(data$sepal ~ data$species)

aggregate(data = data, sepal ~ species, FUN = "mean") # 평균
aggregate(data = data, sepal ~ species, FUN = "var") # 분산

# 등분산성 검토 Bartllet test 사용
bartlett.test(sepal ~ species, data = data)

# > bartlett.test(sepal ~ species, data = data)

#         Bartlett test of homogeneity of variances

# data:  sepal by species
# Bartlett's K-squared = 6.8917, df = 1, p-value = 0.00866

# p -value가 0.05이하 이므로 등분산성을 만족하지 못함 -> 분산이 이질적임.
# 귀무가설을 채택 
# 귀무가설 : 분산이 일정하지않음, 대립가설 : 분산이 일정함

t.test(sepal ~ species, var.equal = FALSE, data = data)
