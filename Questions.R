cat("\014")
# Set working directory and import datafiles
setwd("C:/Users/paola/OneDrive/Documents/Data Analytics/Titanic -Data Analytics Project")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Did the passenger class (Pclass) have any impact on the survival rate? 
#Is there a significant difference in survival rates between different passenger classes?

library(dplyr)

survival_by_class <- train %>%
  group_by(Pclass) %>%
  summarise(Survival_Rate = mean(Survived))

survival_by_class

#How does the fare (ticket price) vary across different passenger classes? 
#Is there a correlation between fare and survival?

library(ggplot2)

ggplot(train, aes(x = Pclass, y = Fare, color = factor(Survived))) +
  geom_boxplot() +
  labs(x = "Passenger Class", y = "Fare", color = "Survived")

#Were passengers traveling alone more likely to survive compared to those traveling 
#with family members? What was the survival rate for solo travelers versus those with family?

titanic <- train %>%
  mutate(Family_Size = SibSp + Parch)

survival_by_family_size <- titanic %>%
  group_by(Family_Size) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE))

survival_by_family_size

#Did the port of embarkation (Embarked) have any influence on the survival rate? 
#Did passengers boarding from different ports have different chances of survival?
  

survival_by_embarked <- train %>%
  group_by(Embarked) %>%
  summarise(Survival_Rate = mean(Survived))

survival_by_embarked

#Did the presence of parents or children (Parch) impact the survival rate? 
#Did passengers with parents or children on board have a higher chance of survival 
#compared to those traveling alone?
  
survival_by_parch <- train %>%
  group_by(Parch) %>%
  summarise(Survival_Rate = mean(Survived))

survival_by_parch

#How does the age distribution differ between survivors and non-survivors?
#Are there any specific age groups that were more likely to survive?
 
library(ggplot2)

ggplot(titanic, aes(x = Age, fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  labs(x = "Age", fill = "Survived")

#Did the survival rate differ between different titles (e.g., Mr., Mrs., Miss, etc.)?
#Were certain titles associated with a higher chance of survival?
 
titanic <- titanic %>%
  mutate(Title = gsub("(.*, )|(\\..*)", "", Name))

survival_by_title <- titanic %>%
  group_by(Title) %>%
  summarise(Survival_Rate = mean(Survived))

survival_by_title

#Did the survival rate vary based on the combination of factors such as gender, age, passenger
#class, and embarked port? Were there any specific groups that had significantly higher or
#lower survival rates?

library(dplyr)

survival_by_factors <- train %>%
  group_by(Sex, Age, Pclass, Embarked) %>%
  summarise(Survival_Rate = mean(Survived))

survival_by_factors

#Did the survival rate vary based on the fare paid by passengers? 
#Were passengers who paid higher fares more likely to survive?

library(ggplot2)

# Subset the dataset to include relevant columns
fare_survival <- train[, c("Survived", "Fare")]

# Create a new column indicating whether the fare is high or not
fare_survival$High_Fare <- ifelse(fare_survival$Fare > quantile(fare_survival$Fare, 0.75), 
                                  "High", "Low")

# Calculate the survival rate by fare category
survival_by_fare <- fare_survival %>%
  group_by(High_Fare) %>%
  summarise(Survival_Rate = mean(Survived))

# Plotting the survival rate by fare category
ggplot(survival_by_fare, aes(x = High_Fare, y = Survival_Rate, fill = High_Fare)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Fare Category", y = "Survival Rate", fill = "Fare Category") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), guide = FALSE) +
  theme_minimal()


library(dplyr)
library(dslabs)
library(ggplot2)

#How many passengers survived the Titanic disaster?

survived_passengers <- train %>% filter(Survived == 1)
num_survivors <- nrow(survived_passengers)
num_survivors

#What is the median age of the passengers?

median_age <- train %>% mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  pull(Age) %>%
  median(na.rm = TRUE)
median_age

#What is the fare range for each passenger class?

fare_range <- train %>% group_by(Pclass) %>%
  summarize(Fare_Range = paste0(min(Fare), "-", max(Fare)))
fare_range

#What is the survival rate for male and female passengers?

survival_rate <- train %>% group_by(Sex) %>%
  summarize(Survival_Rate = mean(Survived))
survival_rate

#How many passengers had siblings/spouses aboard the Titanic?

passengers_with_sibsp <- train %>% filter(SibSp > 0)
num_passengers_with_sibsp <- nrow(passengers_with_sibsp)
num_passengers_with_sibsp

#What is the most common embarkation port?

most_common_port <- train %>% group_by(Embarked) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice(1)
most_common_port

#What is the fare percentile for each passenger class?

fare_percentiles <- train %>% group_by(Pclass) %>%
  summarize(Percentile_25 = quantile(Fare, probs = 0.25),
            Percentile_50 = quantile(Fare, probs = 0.50),
            Percentile_75 = quantile(Fare, probs = 0.75))
fare_percentiles

#What is the average fare paid by passengers who survived compared to those who did not?

fare_comparison <- train %>% group_by(Survived) %>%
  summarize(Average_Fare = mean(Fare))
fare_comparison

#What is the survival rate based on the combination of passenger class and sex?

survival_rate_combination <- train %>% group_by(Pclass, Sex) %>%
  summarize(Survival_Rate = mean(Survived))
survival_rate_combination

#What is the rank of each passenger based on their fare within each passenger class?

train <- train %>% group_by(Pclass) %>%
  mutate(Fare_Rank = rank(Fare))
train

#Create an indicator variable to identify passengers who were traveling alone (no siblings/spouses or parents/children).

train <- train %>% mutate(Alone = ifelse(SibSp == 0 & Parch == 0, 1, 0))

#Create a cumulative distribution function (CDF) plot of passenger ages.

cdf_age <- ecdf(train$Age)
plot(cdf_age, main = "CDF of Passenger Age", xlab = "Age", ylab = "Cumulative Probability")

#Generate a histogram of the fare distribution for each passenger class.

library(ggplot2)
ggplot(train, aes(x = Fare)) +
  geom_histogram(binwidth = 10, fill = "lightblue") +
  facet_wrap(~Pclass) +
  labs(title = "Fare Distribution by Passenger Class", x = "Fare", y = "Frequency")

#Create a scatter plot showing the relationship between age, fare, and survival status.

ggplot(train, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point() +
  labs(title = "Age vs Fare by Survival Status", x = "Age", y = "Fare", color = "Survived")
#Is there a difference in the fare distribution between different passenger classes?
# Plotting the density curves for fare distribution by passenger class
train %>%
  ggplot(aes(x = Fare, fill = factor(Pclass))) +
  geom_density(alpha = 0.5) +
  labs(title = "Fare Distribution by Passenger Class", x = "Fare", y = "Density",
       fill = "Passenger Class")

#-- Was the oldest passenger a female or male? -- 
# By calculation the average age for each sex
females_age<-train %>% filter (Sex=='female' & !is.na(Age)) %>% select(Age)
# By calculation the average age for each sex
sum(females_age$Age)/nrow(females_age)

males_age<-train %>% filter (Sex=='male' & !is.na(Age)) %>% select(Age)
sum(males_age$Age)/nrow(males_age)



#Q Calculate survival rate by embarkation port
survival_rate_by_port <- aggregate(Survived ~ Embarked, data = train, FUN = mean)
survival_rate_by_port

#Q Create a new column to categorize passengers as crew, adults, or children
train$PassengerCategory <- ifelse(is.na(train$Age), "Crew",
                                  ifelse(train$Age < 18, "Child", "Adult"))

# Calculate survival rate by passenger category
survival_rate_by_category <- aggregate(Survived ~ PassengerCategory, data = train, FUN = mean, na.action = na.pass)
survival_rate_by_category















