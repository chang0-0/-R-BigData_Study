data("airquality")
ds <- airquality

summary(ds)
ds <- na.omit(ds)
quantile(ds$Solar.R)
ds_temp <- ds$Solar.R

ds_temp <- ifelse(ds$Solar <= 113.5 | ds$Solar >= 255.5, 0, ds_temp)
print(ds_temp)
sum_stat <- mean(ds_temp) + sd(ds_temp)
print(sum_stat)

dt_airquality <- ds[c(1:nrow(ds*0.9)),]

print(dt_airquality)

mean_train <- mean(dt_airquality$Ozone, na.rm = TRUE)
print(mean_train)

median_before <- median(dt_airquality$Ozone, na.rm = TRUE)

dt_airquality$Ozone[is.na(dt_airquality$Ozone)] <- mean_train

median_after <- median(dt_airquality$Ozone, na.rm = TRUE)

diff_median <- abs(median_before - median_after)
print(diff_median)
