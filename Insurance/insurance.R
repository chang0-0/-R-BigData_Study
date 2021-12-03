ds <- read.csv("./insurance.csv")
summary(ds)

colSums(is.na(ds))
charges_mean <- mean(ds$charges)
print(charges_mean)
outlier <- 1.5*sd(ds$charges)
print(outlier)

outlier_upper <- charges_mean + outlier
outlier_lower <- charges_mean - outlier

result <- ds %>% filter(charges >= outlier_upper | charges <= outlier_lower) %>% summarise(sum = sum(charges))
print(result)
