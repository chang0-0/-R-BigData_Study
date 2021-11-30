data("airquality")
ds <- airquality
summary(ds)

dt_airquality <- ds[c(1:nrow(ds*0.9)),]

print(dt_airquality)

mean_train <- mean(dt_airquality$Ozone, na.rm = TRUE)
print(mean_train)

median_before <- median(dt_airquality$Ozone, na.rm = TRUE)

diff_median <- abs(median_before - median_after)
print(diff_median)
