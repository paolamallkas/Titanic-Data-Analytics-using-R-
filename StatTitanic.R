setwd("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset")
data("Titanic")
train <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/train.csv")
test <- read.csv("C:/Users/megim/OneDrive/Documents/CIT/Master/2nd Semester/Data Science/Project/Titanic/Dataset/test.csv")
str(train)

# Create a contingency table of Pclass and Survived
contingency <- table(train$Pclass, train$Survived)

# Perform chi-squared test of independence
chi_test <- chisq.test(contingency)

# Extract the correlation coefficient (phi coefficient) from the test result
correlation <- sqrt(chi_test$statistic / sum(contingency))

# Print the correlation coefficient
correlation

# Obtain the critical value for the chi-squared test
critical_value <- qchisq(0.95, df = chi_test$parameter)

# Print the critical value
critical_value

# Obtain the chi-squared statistic value
chi_squared <- chi_test$statistic

# Print the chi-squared statistic value
chi_squared




# Create a contingency table of Sex and Survived
contingency <- table(train$Sex, train$Survived)

# Perform chi-squared test of independence
chi_test <- chisq.test(contingency)

# Extract the correlation coefficient (phi coefficient) from the test result
correlation <- sqrt(chi_test$statistic / sum(contingency))

# Print the correlation coefficient
correlation
# Obtain the critical value for the chi-squared test
critical_value <- qchisq(0.95, df = chi_test$parameter)

# Print the critical value
critical_value

# Obtain the chi-squared statistic value
chi_squared <- chi_test$statistic

# Print the chi-squared statistic value
chi_squared



# Create a contingency table of Cabin and Survived
contingency <- table(train$Cabin, train$Survived)

# Perform chi-squared test of independence
chi_test <- chisq.test(contingency)

# Extract the correlation coefficient (phi coefficient) from the test result
correlation <- sqrt(chi_test$statistic / sum(contingency))

# Print the correlation coefficient
correlation

# Obtain the critical value for the chi-squared test
critical_value <- qchisq(0.95, df = chi_test$parameter)

# Print the critical value
critical_value

# Obtain the chi-squared statistic value
chi_squared <- chi_test$statistic

# Print the chi-squared statistic value
chi_squared
