library(dplyr)
data(ChickWeight)
main.ds <- ChickWeight
head(main.ds)


normal <- function(x) {
    reuslt = (x - min(x)) / (max(x) - min(x))
    return(reuslt)
}


temp <- normal(main.ds$weight)
temp <- as.data.frame(temp)

temp <- temp %>% filter(temp >= 0.5)
result2 <- nrow(temp)
print(result2)
