
setwd("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset")
data("Titanic")
train <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/train.csv")
test <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/test.csv")

# Look at gender patterns
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)   #Get row proportions

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

# Look at age patterns
summary(train$Age)
# Create a new variable child
train$Child <- 0
train$Child[train$Age < 18] <- 1
train
# No of survived passangers in each subset
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# Total number of passengers in each subset
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# Proportions
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Look at class and fare patterns  (8, 9)
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also die
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)

