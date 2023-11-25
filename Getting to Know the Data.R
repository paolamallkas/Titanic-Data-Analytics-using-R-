# Set working directory and import datafiles
#Load the Titanic dataset
setwd("C:/Users/paola/OneDrive/Documents/Data Analytics/Titanic -Data Analytics Project")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Install packages
#psych, GGally, dplyr, ggplot2, rpart, rpart.plot, Amelia

library(GGally)
library(psych)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(Amelia)

#Cleaning and preparing the dataset
View(train)
library(Amelia)
missmap(train, col = c("black", "grey"))

# Examine structure of dataframe
str(train)
head(train)
length(train)
# Summary statistics
summary(train)

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

#How many observations (passengers) are there in the dataset?
nrow(train)
#How many variables (columns) are there in the dataset?
 ncol(train)
#What is the survival rate among the passengers?
mean(train$Survived)
#What is the average age of the passengers?
mean(train$Age, na.rm = TRUE)
#How many male and female passengers are there?
table(train$Sex)
#What is the highest fare paid by a passenger?
 max(train$Fare)
#How many passengers embarked from each port (S, C, Q)?
table(train$Embarked)
#How many passengers had siblings/spouses aboard (SibSp) and parents/children aboard (Parch)?
table(train$SibSp)
table(train$Parch)
#---------------------------------------------------------------------------------------

#The gender-class model

# Look at gender patterns
summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

# Look at age patterns
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Look at class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0



