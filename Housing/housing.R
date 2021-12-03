ds <- read.csv("./housing.csv")

summary(ds)
nrow(ds)
# 순서대로 80% 데이터 추출 훈련 데이터로 추출
ds80 <- nrow(ds) * 0.8
ds <- ds[1:ds80,]

nrow(ds)

bedroom_median <- median(ds$total_bedrooms, na.rm=TRUE)

ds_after <- ds %>% mutate(total_bedrooms = ifelse(is.na(ds$total_bedrooms), median(ds$total_bedrooms, na.rm=TRUE), ds$total_bedroom))

after_sd <- sd(ds_after$total_bedrooms)

colSums(is.na(ds))
before_sd <- sd(ds$total_bedrooms, na.rm=TRUE)

result <- abs(after_sd - before_sd)
print(result)


