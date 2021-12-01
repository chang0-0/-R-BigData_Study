library(dplyr)
library(readcsv)
install.packages("readxl")


list.files()

csv_data <- read.csv("./sales_train_v2.csv", header = T, fileEncoding="UTF-8-BOM")
summary(csv_data)
