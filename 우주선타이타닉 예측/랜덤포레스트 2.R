library(tidyverse) # metapackage of all tidyverse packages
library(Hmisc) # To deal with missing Values
library(randomForest) #machine learning algorithm
library(caret)
library(skimr)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/우주선타이타닉 예측")

train <- read.csv("train.csv", stringsAsFactors = TRUE, na.strings = c("", NA), sep =",", header = TRUE)
test <- read.csv("test.csv", stringsAsFactors = TRUE, na.strings = c("", NA), sep = ",", header = TRUE)


skim_without_charts(train)
skim_without_charts(test)
str(train)

table(train$HomePlanet)

temp_train_data<-train
temp_test_data<-test

temp_train_data$s_id<-substr(train$PassengerId,0,4)
temp_train_data<-temp_train_data%>%group_by(s_id)%>%mutate(Group_count=n())
temp_train_data$Group_count<-as.character(temp_train_data$Group_count)

temp_test_data$s_id<-substr(test$PassengerId,0,4)
temp_test_data<-temp_test_data%>%group_by(s_id)%>%mutate(Group_count=n())
temp_test_data$Group_count<-as.character(temp_test_data$Group_count)

train$Group_count<-temp_train_data$Group_count
train$s_id<-temp_train_data$s_id
test$Group_count<-temp_test_data$Group_count
test$s_id<-temp_test_data$s_id

#Remmoving the temporary data sets
rm(temp_train_data,temp_test_data)


#Filling missing ages
train$Age<-impute(train$Age,mean)
test$Age<-impute(test$Age,mean)
train$Age<-as.numeric(train$Age)
test$Age<-as.numeric(test$Age)

x<-c(2,3,4,5,7)
for (j in x)
{
m<-which(is.na(train[,j]))
for (i in m)
    {
    if(train[i,15]>1)
        {
        if(train[i,16]==train[i-1,16] & is.na(train[i-1,j]))
            {
             train[i,j]<-train[i-1,j]
            }
        else if(train[i,16]==train[i+1,16] & is.na(train[i-1,j]))
            {
            train[i,j]<-train[i+1,j]
            }
        }
    }
}


rm(x,j,m)
x<-c(2,3,4,5,7)
for (j in x)
{
m<-which(is.na(test[,j]))
for (i in m)
    {
     if(test[i,14]>1)
        {
        if(test[i,15]==test[i-1,15] & is.na(test[i-1,j]))
            {
             test[i,j]<-test[i-1,j]
            }
        else if(test[i,15]==test[i+1,15] & is.na(test[i-1,j]))
            {
            test[i,j]<-test[i+1,j]
            }
        }
}
    }



#Filling rest of the missing data


train<-train%>%fill(Destination,Cabin,HomePlanet,CryoSleep, .direction="updown")
test<-test%>%fill(Destination,Cabin,HomePlanet,CryoSleep, .direction="updown")
train$VIP[is.na(train$VIP)] <- "FALSE" 
test$VIP[is.na(test$VIP)] <- "FALSE"

#Creating Hode-Destination (HD_pairs)
train$HD_pair<-str_c(train$HomePlanet, " ", train$Destination)
test$HD_pair<-str_c(test$HomePlanet," ", test$Destination)

#plot for cabins
train$Cabin_code<-substr(train$Cabin,0,1)
train$SC_code<-substr(train$Cabin,3,3)


#Creating cabin_code,SC_code and SSC_code in test data as well
test$Cabin_code<-substr(test$Cabin,0,1)
test$SC_code<-substr(test$Cabin,3,3)
test$SSC_code<-substr(test$Cabin,5,5)


#Bin the age for train and test data
train$Age_bins <- cut(train$Age, breaks=c(-Inf,12,18,60,Inf), labels=c("Child","Adolescent","Adult","Senior"))
test$Age_bins <- cut(test$Age, breaks=c(-Inf,12,18,60,Inf), labels=c("Child","Adolescent","Adult","Senior"))

#Plotting the binned age for Train_data
ggplot(train)+geom_bar(aes(Age_bins,fill=Transported),position="dodge2")


train$RoomService[is.na(train$RoomService)] <- 0
train$FoodCourt[is.na(train$FoodCourt)] <- 0
train$ShoppingMall[is.na(train$ShoppingMall)] <- 0
train$Spa[is.na(train$Spa)] <- 0
train$VRDeck[is.na(train$VRDeck)] <- 0

#Mutating Expenditures into 1
train<-train%>%mutate(Expenditure=RoomService+FoodCourt+ShoppingMall+Spa+VRDeck)

#We will bin the expenditure data into 2 groups
train$RoomService_bin <- cut(train$RoomService, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
train$FoodCourt_bin <- cut(train$FoodCourt, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
train$ShoppingMall_bin <- cut(train$ShoppingMall, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
train$Spa_bin <- cut(train$Spa, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
train$VRDeck_bin <- cut(train$VRDeck, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))


test$RoomService[is.na(test$RoomService)] <- 0
test$FoodCourt[is.na(test$FoodCourt)] <- 0
test$ShoppingMall[is.na(test$ShoppingMall)] <- 0
test$Spa[is.na(test$Spa)] <- 0
test$VRDeck[is.na(test$VRDeck)] <- 0

#We will create a total expenditure data into test group too
test<-test%>%mutate(Expenditure=RoomService+FoodCourt+ShoppingMall+Spa+VRDeck)

#We will bin the expenses data into 2 groups
test$RoomService_bin <- cut(test$RoomService, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
test$FoodCourt_bin <- cut(test$FoodCourt, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
test$ShoppingMall_bin <- cut(test$ShoppingMall, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
test$Spa_bin <- cut(test$Spa, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))
test$VRDeck_bin <- cut(test$VRDeck, breaks=c(-Inf,0,Inf), labels=c("FALSE","TRUE"))


#Dropping Columns we won't use
train<-train%>%select(-PassengerId, -Name,-s_id)
test<-test%>%select(-Name,-s_id)

#Changing Trasnported (since it is a classification problem) and Age_bins to factor
train$Transported<-as.factor(train$Transported)


set.seed(1234)
transported_equation <- "Transported ~ HomePlanet+CryoSleep+HD_pair+Group_count+Cabin_code+Age+RoomService+FoodCourt+ShoppingMall+Spa+VRDeck+SC_code"
transported_formula <- as.formula(transported_equation)
titanic_space_model <- randomForest(formula=transported_formula, data = train, ntree =  500, mtry = 4, nodesize = .01 * nrow(train))

titanic_space_model
print(importance(titanic_space_model))
plot(titanic_space_model,main="Error Rate",log="y")
forest.pred <- predict(titanic_space_model, newdata = test)

val <- read.csv("submission.csv")
val$Transported <- as.factor(as.character(val$Transported))
val$Transported <- factor(val$Transported, levels = c("FALSE", "TRUE"))
levels(val$Transported)

# Binding the Passenger ID
PassengerInfo <- test %>% select(PassengerId)

new_df <- data.frame(PassengerInfo, Transported)
new_df$Transported <- ifelse(new_df$Transported == TRUE, "TRUE", "FALSE")
new_df$Transported <- as.factor(new_df$Transported)
levels(new_df$Transported)

caret::confusionMatrix(data = new_df$Transported, ref = "Transported" )

str(new_df$Transported)
str(val$Transported)

write_csv(new_df, "randomForest.csv")

