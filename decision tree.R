#Loading the Packages
library(dplyr)
library(ggplot2)
library(lattice)
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
#-----

Titanic_train_tree <- train_data %>%
  select(-c(PassengerId)) %>%
  mutate(Survived = factor(Survived))

train.control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE
)

tree_model <- train(
  make.names(Survived) ~ .,
  data = Titanic_train_tree,
  method = "rpart2",
  trControl = train.control,
  tuneLength = 10,
  metric = "ROC"
)

library(ggplot2)
ggplot(tree_model) +
  geom_line(color = "black") +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(x = maxdepth, y = ROC, label = round(ROC, 2)),
            family = "Palatino", vjust = -1, size = 3) +
  labs(title = "ROC by Maximum Tree Depth")

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(tree_model$finalModel, type = 5, box.palette = "BuGn",
           pal.thresh = 0.5, cex = 1.5, split.family = "Palatino",
           nn.family = "Palatino", fam.main = "Palatino", family = "Palatino",
           round = 0, pal.node.fun = TRUE)

#Comparing Results

Titanic_train_tree$survival_pred <- predict(tree_model, Titanic_train_tree)
model_prob <- predict(tree_model, newdata = Titanic_train_tree, type = "prob")

Titanic_train_tree <- Titanic_train_tree %>%
  mutate(survival_prob = model_prob[,2]) %>%
  mutate(survival_pred = ifelse(survival_pred == "X0", 0, 1)) %>%
  mutate(survival_pred = factor(survival_pred))

cfMatrix_tree <- confusionMatrix(
  data = relevel(Titanic_train_tree$survival_pred, ref = "1"),
  reference = relevel(Titanic_train_tree$Survived, ref = "1")
)

cfMatrix_tree
#__________________________________________________________________________________________

cfMatrix_tree_plot <- as.data.frame(cfMatrix_tree$table)
cfMatrix_tree_plot <- cfMatrix_tree_plot %>%
  mutate(Prediction = fct_relevel(Prediction, "1", after = 1),
         Reference = fct_relevel(Reference, "1", after = 1)) %>%
  group_by(Prediction) %>%
  mutate(Accuracy_prediction = Freq/sum(Freq)) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(Accuracy_reference = Freq/sum(Freq))


cfMatrix_tree_plot %>%
  ggplot(aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(x = Prediction, y = Reference, label = Freq),
            family = "Palatino", color = "white", size = 20)  +
  labs(title = "Confusion Matrix")
#____________________________________________________________________________________

