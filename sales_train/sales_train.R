library(dplyr)
library(readcsv)

getwd()
setwd("C:\\Users\\Samsung\\Desktop\\빅분기실기준비\\sales_train")
list.files()
csv_data <- read.csv("./sales_train_v2.csv", header = T)
summary(csv_data)

# 그룹별 행의 갯수 세기 -> summarise(n = n()) OR tally()
dt_cnt <- csv_data %>% group_by(item_id) %>% tally()
print(dt_cnt)

test <- csv_data %>% group_by(item_id)
test

test2 <- csv_data %>% tally()
print(test2)

dt_cnt <- csv_data %>% count(item_id)
dt_arr <- dt_cnt %>% arrange(desc(n))
dt_arr

top3_sd <- csv_data %>% filter(item_id %in% c(dt_arr[1,1], dt_arr[2, 1],dt_arr[3,1])) %>% summarise(sd = sd(item = price))
print(top3_sd)
