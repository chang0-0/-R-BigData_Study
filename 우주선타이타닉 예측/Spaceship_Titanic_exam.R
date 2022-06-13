library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(Hmisc) # To deal with missing Values
library(randomForest) #machine learning algorithm
library(caret)
library(skimr)
library(ggcorrplot)

getwd()
test <- read.csv("test.csv")
train <- read.csv("train.csv")

test$Transported <- NA
all = rbind(train, test)
all %>% head(3)

# split PassengerId into two parts: Group and Number
temp = all$PassengerId %>% str_split(pattern="_", simplify=TRUE)
all$IdGroup = temp[, 1] %>% as.integer()
all$IdNum = temp[, 2] %>% as.integer()

# split Cabin into three parts: Deck, Number and Side
temp = all$Cabin %>% str_split(pattern="/", simplify=TRUE)
all$CabinDeck = temp[, 1] %>% as.factor()
all$CabinNum = temp[, 2] %>% as.integer()
all$CabinSide = temp[, 3] %>% str_replace(pattern="P", replace="Port") %>% str_replace(pattern="S", replace="StarBoard") %>% as.factor()

# split Name into two parts: FirstName and LastName (these features are not used in this Notebook)
temp = all$Name %>% str_split(pattern=" ", simplify=TRUE)
all$FirstName = temp[, 1]
all$LastName = temp[, 2]

# drop splitted columns
all = all[, !colnames(all) %in% c("PassengerId", "Cabin", "Name")]

# convert "True" and "False" to 1 and 0 respectively
all$CryoSleep = as.integer(all$CryoSleep == "True")
all$VIP = as.integer(all$VIP == "True")
all$Transported = as.integer(all$Transported == "True")

all %>% head(3)

# fill NA with 0
all$RoomService = all$RoomService %>% replace_na(0)
all$FoodCourt = all$FoodCourt %>% replace_na(0)
all$ShoppingMall = all$ShoppingMall %>% replace_na(0)
all$Spa = all$Spa %>% replace_na(0)
all$VRDeck = all$VRDeck %>% replace_na(0)

# fill NA with a specific value
all$Age = all$Age %>% replace_na(mean(all$Age, na.rm=TRUE))
all$CabinNum = all$CabinNum %>% replace_na(-1)

# one-hot encoding of factors
encoder = dummyVars(~ HomePlanet + Destination + CabinDeck + CabinSide, data=all, sep="") 
all.dummy = predict(encoder, newdata=all)
colnames(all.dummy)[5:8] = c("Destination", "DestinationC", "DestinationP", "DestinationT")
all.dummy %>% head(3)




# drop factor columns
factors = c("HomePlanet", "Destination", "CabinDeck", "CabinSide", "FirstName", "LastName")
all.factor = all[factors]
all = all[!colnames(all) %in% factors]

# bind encoded columns
all = cbind(all, all.dummy)

# drop columns to avoid rank deficiency
drop.cols = c("HomePlanet", "Destination", "CabinDeck", "CabinSide", "CabinSideStarBoard")
all = all[!colnames(all) %in% drop.cols]

all %>% head(3)

# count NA
train_na = apply(all[!is.na(all$Transported),], MARGIN=2, function(x){sum(is.na(x))})
test_na = apply(all[is.na(all$Transported),], MARGIN=2, function(x){sum(is.na(x))})
temp = matrix(c(train_na, test_na), nrow=length(train_na), dimnames=list(names(train_na)))
data.frame(Data=c("TRAIN", "TEST"), t(temp))

# split dataframe into two parts: train and test
train = all[!is.na(all$Transported),]; train.factor = all.factor[!is.na(all$Transported),]
test = all[is.na(all$Transported),]; test.fa = all.factor[is.na(all$Transported),]

options(repr.plot.width=10, repr.plot.height=10)
ggcorrplot(cor(train), hc.order=TRUE, outline.col="white", type="lower",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
