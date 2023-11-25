# Load the required libraries
library(rpart)
library(rpart.plot)

# Read the train.csv file from the Titanic dataset (adjust the file path accordingly)
train <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/train.csv")

# Remove rows with missing values
train <- na.omit(train)

# Function to calculate information gain
calculate_information_gain <- function(dataset, feature, target) {
  # Calculate the overall entropy
  overall_entropy <- entropy(dataset[[target]])
  
  # Calculate the weighted average entropy for each value of the feature
  weighted_entropy <- sapply(unique(dataset[[feature]]), function(value) {
    subset_data <- dataset[dataset[[feature]] == value, ]
    entropy_value <- entropy(subset_data[[target]])
    proportion <- nrow(subset_data) / nrow(dataset)
    proportion * entropy_value
  })
  
  # Calculate the information gain
  information_gain <- overall_entropy - sum(weighted_entropy)
  
  return(information_gain)
}

# Function to calculate entropy
entropy <- function(vector) {
  proportions <- table(vector) / length(vector)
  entropy <- -sum(proportions * log2(proportions))
  return(entropy)
}

# Calculate information gain for each attribute
attributes <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
information_gains <- sapply(attributes, function(attribute) {
  calculate_information_gain(train, attribute, "Survived")
})

# Find the attribute with the highest information gain
root_attribute <- attributes[which.max(information_gains)]
root_information_gain <- information_gains[which.max(information_gains)]

# Print the information gain for each attribute
cat("Information Gains:\n")
for (i in seq_along(attributes)) {
  cat(attributes[i], ": ", information_gains[i], "\n")
}

# Print the attribute with the highest information gain
cat("Attribute with Highest Information Gain:", root_attribute, "\n")
cat("Information Gain for Root Attribute:", root_information_gain, "\n")

# Create a subset of the data using only the attribute with the highest information gain
subset_train <- train[, c(root_attribute, "Survived")]

# Build the decision tree using rpart with the attribute with highest information gain as the root
fit <- rpart(Survived ~ ., data = subset_train, method = "class")

# Plot the decision tree
rpart.plot(fit)

