#Loading the Packages
library(randomForest)
library(dplyr)
library(caret)
library(readr)

#Loading the Test file and reading the columns.

test_data <- read.csv("C:/Users/paola/OneDrive/Documents/Data Analytics/Titanic -Data Analytics Project/test.csv", 
                      stringsAsFactors = FALSE, header = TRUE)
head(test_data)

#Loading the Training file and reading the columns.

train_data <- read.csv("C:/Users/paola/OneDrive/Documents/Data Analytics/Titanic -Data Analytics Project/train.csv", stringsAsFactors = FALSE, header = TRUE)
head(train_data)

#Missing values from the train_data
nrow(train_data)
sum(is.na(train_data))
sum(is.na(train_data$Age))

table(train_data$Age)

#Missing Values from the test_data
nrow(test_data)
sum(is.na(test_data))
sum(is.na(test_data$Age))

#Filling missing values for Age
median(train_data$Age, na.rm=TRUE)
median(test_data$Age, na.rm=TRUE)
train_data$Age  <- ifelse(is.na(train_data$Age), 28, train_data$Age)
test_data$Age  <- ifelse(is.na(test_data$Age), 27, test_data$Age)

#Identify missing Fare and fill missing value
subset(test_data, is.na(test_data$Fare))
thrd_cl_fr <- subset(test_data, c(test_data$Pclass==3, test_data$Embarked=="S"))
m_fare <- round(median(thrd_cl_fr$Fare, na.rm=TRUE),2)
m_fare
test_data$Fare <- ifelse(is.na(test_data$Fare), m_fare, test_data$Fare)

#Identify and fill missing Embark values
table(train_data$Embarked)
table(test_data$Embarked)

m_embarked <-subset(train_data, train_data$Embarked=="")
m_embarked

train_data[train_data$Embarked=="", "Embarked"] <- "C"



