
setwd("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset")
data("Titanic")
train <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/train.csv")
test <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/test.csv")

# Structure 
str(train)

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0, 418)

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "alldie.csv", row.names = FALSE)

