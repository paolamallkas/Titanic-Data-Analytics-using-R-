#reading test and train files
setwd("C:/Users/paola/OneDrive/Documents/Data Analytics/Titanic -Data Analytics Project")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

library(dplyr)
library(ggplot2)

#Explore the data for patterns of surviviability

women <- subset(train, train$Sex=="female")
count(women)
n_women <- count(women)

sur_women <- subset(women, women$Survived==1)
count(sur_women)
n_sur_women <- count(sur_women)

n_sur_women/n_women

men <- subset(train, train$Sex=="male")
count(men)
n_men <- count(men)
sur_men <- subset(men, men$Survived==1)
count(sur_men)
n_sur_men <- count(sur_men)

n_sur_men/n_men

#1. Create a histogram of survival status

library(ggplot2)

ggplot(data = train, aes(x = Survived, fill = factor(Survived))) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("tomato3", "slateblue4"), labels = c("Not Survived", "Survived")) +
  labs(x = "Survival Status", y = "Count", title = "Survived vs Not Survived Histogram") +
  stat_bin(aes(label = ifelse(after_stat(count) != 0, paste0(round(after_stat(count) / sum(after_stat(count)) * 100), "%"), "")),
           geom = "text", vjust = -0.5, binwidth = 0.5) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Not Survived", "Survived"))


#2. plass - number of passangers per class - histogram of three classes
ggplot(data=train, aes(x=Pclass, fill = factor(Pclass))) + 
  geom_bar(position = "dodge") + 
  geom_text(stat='count', aes(label=..count..), position = position_dodge(0.9), vjust=-0.2) +
  ylab("Number of Passengers") +
  scale_fill_manual(values = c("powderblue", "violet", "seagreen"))

#histogram of three classes between survived or not

ggplot(data = train, aes(x = factor(Survived), fill = factor(Pclass))) +
  geom_bar(position = "dodge") +
  stat_count(aes(label = paste0(round(..count../sum(..count..) * 100), "%")),
             position = position_dodge(width = 0.9), vjust = -0.5, color = "white") +
  labs(x = "Survived", y = "Number of Passengers", fill = "Class") +
  scale_fill_manual(values = c("powderblue", "violet", "seagreen")) +
  scale_x_discrete(labels = c("Did not survive", "Survived")) +
  theme_minimal()


#Survived and sex

# Create the histogram of male and female passengers
ggplot(data = train, aes(x = factor(Sex), fill = factor(Sex))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("lightpink3", "lightblue3")) +
  labs(x = "Sex", y = "Count", fill = "Sex") +
  ggtitle("Distribution of Male and Female Passengers") +
  theme_minimal()

ggplot(data = train, aes(x = factor(Survived), fill = factor(Sex))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.5, color = "white") +
  labs(x = "Survived", y = "Number of Passengers", fill = "Sex") +
  scale_fill_manual(values = c("lightpink3", "lightblue3")) +
  scale_x_discrete(labels = c("Did not survive", "Survived")) +
  theme_minimal()

#Embarked / number of passengers

ggplot(data = train, aes(x = Embarked, fill = Embarked)) +
  geom_bar() +
  labs(x = "Embarked", y = "Number of Passengers") +
  scale_fill_manual(values = c("C" = "lightblue4", "Q" = "burlywood3", "S" = "darkseagreen4")) +
  theme_minimal()

#Histogram of Embarked between survived or not

ggplot(data = train, aes(x = factor(Survived), fill = Embarked)) +
  geom_bar(position = "dodge") +
  labs(x = "Survived", y = "Number of Passengers", fill = "Embarked") +
  scale_fill_manual(values = c("C" = "lightblue4", "Q" = "burlywood3", "S" = "darkseagreen4")) +
  scale_x_discrete(labels = c("Did not survive", "Survived")) +
  theme_minimal()

# Create a histogram of Fare with separated bars for Survived and Not Survived passengers
library(ggplot2)  #firebrick", "dodgerblue4

# Categorize the Fare into different categories
fare_categories <- cut(train$Fare, breaks = c(0, 10, 50, 100, max(train$Fare)),
                       labels = c("low", "mid", "high_mid", "high"), include.lowest = TRUE)

# Add the Fare_Category column to the data frame
train$Fare_Category <- fare_categories

# Plot the histogram
histogram_plot <- ggplot(train, aes(x = Fare_Category, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "indianred2", "1" = "lightsteelblue4")) +
  labs(x = "Fare Category", y = "Count", title = "Histogram of Fare by Survival") +
  theme_bw()

print(histogram_plot)



#Age
ggplot(data=train, aes(x=Age,)) + 
  geom_histogram(binwidth = 5) +
  xlab("Age")
#Survival by Age

#Age and Sex
ggplot(train) + geom_freqpoly(mapping = aes(x = Age, color = Sex), binwidth = 2.5) +
  ylab("Frequency")


filtered_data <- train[!is.na(train$Age), ]

# Create a histogram of the Age distribution
library(ggplot2)

data <- read.csv("train.csv")  

ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "cyan4", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Create separate datasets for not survived and survived individuals
not_survived <- data[data$Survived == 0, ]
survived <- data[data$Survived == 1, ]

# Plot the histogram with separate lines for not survived and survived age distributions
ggplot() + 
  geom_density(data = not_survived, aes(x = Age, color = "Not Survived"), adjust = 1.5) +
  geom_density(data = survived, aes(x = Age, color = "Survived"), adjust = 1.5) +
  scale_color_manual(values = c("lightsteelblue4", "indianred2")) +
  xlab("Age") +
  ylab("Density") +
  ggtitle("Age Distribution of Survivors and Non-Survivors") +
  theme_minimal()

# Create a histogram with colored bins based on survival status
ggplot(train, aes(x = Age, fill = Survival)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Age", y = "Count") +
  scale_fill_manual(values = c("lightsteelblue4", "indianred2"), labels = c("Died", "Survived")) +
  theme_minimal()

# Create the boxplot
ggplot(train, aes(x = factor(Survived), y = Age)) +
  geom_boxplot(fill = "peru", color = "black") +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
  ) +
  labs(title = "Survival Rates by Age", x = "Survived", y = "Age")


#-----------------------------------------------------------------------------------------
# Feature Engineering for Tittle

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# What's in a name?
train$Name[1]

# Join together the test and train sets for easier feature engineering
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

# Engineered variable: Title
combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2]  # Won't work!
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Inspect new feature
table(combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

#-------------------------------------------------------------------------------------------
library(ggplot2)


# Define the colors for each title
title_colors <- c(
  "darkmagenta", "darkmagenta", "aquamarine3", "bisque4",
  "darkgreen", "darkgray", "darkblue", "firebrick2", "darkgreen",
  "darkmagenta", "darkmagenta"
)

# Create a subset of the combined dataset with non-null titles
title_data <- combi[!is.na(combi$Title), ]

# Generate a histogram of titles with custom colors
ggplot(title_data, aes(x = Title, fill = Title)) +
  geom_bar() +
  labs(title = "Histogram of Titles") +
  xlab("Title") +
  ylab("Count") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = title_colors)

#histogram of Titles between survived or not
library(ggplot2)

# Create a subset of the combined dataset with non-null titles and survival information
title_data <- combi[!is.na(combi$Title) & !is.na(combi$Survived), ]

# Generate a histogram of titles based on survival
ggplot(title_data, aes(x = factor(Survived), fill = Title)) +
  geom_bar(position = "dodge") +
  labs(title = "Histogram of Titles by Survival",
       x = "Survived",
       y = "Count") +
  scale_fill_manual(values = c("darkmagenta", "darkmagenta", "aquamarine3", "bisque4",
                               "darkgreen", "darkgray", "darkblue", "firebrick2", "darkgreen",
                               "darkmagenta", "darkmagenta")) +
  theme_minimal()
#------------------------------------------------------------------------------------------
#Feature Engineering for Family

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)
#-------------------------------------------------------------------------------------------
library(ggplot2)

# Create a subset of the train dataset with non-null FamilySize values
train_data <- train[!is.na(train$FamilySize), ]

# Define the categories for FamilySize
train_data$FamilySizeCategory <- cut(train_data$FamilySize,
                                     breaks = c(0, 1, 4, Inf),
                                     labels = c("0", "1", "2"))

# Generate a histogram of FamilySize with different bar colors
ggplot(train_data, aes(x = FamilySizeCategory, fill = FamilySizeCategory)) +
  geom_bar() +
  labs(title = "Histogram of FamilySize",
       x = "Family Size Category",
       y = "Count") +
  scale_fill_manual(values = c("dodgerblue4", "tan4", "olivedrab4")) +
  theme_minimal()


# Create a subset of the train dataset with non-null FamilySize and Survived values
train_data <- train[!is.na(train$FamilySize) & !is.na(train$Survived), ]

# Define the categories for FamilySize
train_data$FamilySizeCategory <- cut(train_data$FamilySize,
                                     breaks = c(0, 1, 4, Inf),
                                     labels = c("0", "1", "2"))

# Generate a histogram of FamilySize based on survival status
ggplot(train_data, aes(x = factor(Survived), fill = FamilySizeCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Histogram of FamilySize by Survival",
       x = "Survived",
       y = "Count") +
  scale_x_discrete(labels = c("Not Survived", "Survived")) +
  scale_fill_manual(values = c("dodgerblue4", "tan4", "olivedrab4")) +
  theme_minimal()

#___________________________________________________________________________________________

# Create the bar plot of Survival Rates by Sex and Pclass
ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(width = 0.4) +
  facet_wrap(~ Pclass) +
  scale_fill_manual(values = c("brown", "steelblue4")) +  # Set the fill colors to red and blue
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
  ) +
  labs(title = "Survival Rates by Sex and Passenger Class", x = NULL, y = "Passenger Count")

# Create the histogram plot - survival rates by sex and passenger class
ggplot(train, aes(x = Age, fill = factor(Survived))) +
  geom_histogram() +
  facet_wrap(~ Sex + Pclass) +
  scale_fill_manual(values = c("brown", "steelblue4")) +  
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
  ) +
  labs(title = "Survival Rates by Age, Sex, and Passenger Class")










