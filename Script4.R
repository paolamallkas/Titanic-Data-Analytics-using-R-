
setwd("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset")
data("Titanic")
train <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/train.csv")
test <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/test.csv")

# Install and load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# What's in a name?
train$Name[1]

# Join together the test and train sets
# rbind- row bind
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)
# What's in a name, again?
combi$Name[1]

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# New variable: Title
combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2]  # Won't work
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
# substitute the first occurrence of a space with nothing
combi$Title <- sub(' ', '', combi$Title)
# Inspect new variable
table(combi$Title)
# Combine small title groups
# Mademoiselle and Madame are similar
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# New variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Combining the Surname with the family size 
#Extract passengers' last name
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new variable
table(combi$FamilyID)
# Delete incorrect family IDs (less than 2)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with our new variables
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
#Incorporating information gain
fit1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class", parms=list(split="information"))
fancyRpartPlot(fit)
fancyRpartPlot(fit1)
# Make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "newtree.csv", row.names = FALSE)

