---
title: Mining Titanic Dataset
author: Colin Manuel Fernandes - 914777
output:
  pdf_document: 
    fig_caption: yes
    highlight: espresso
    number_sections: yes
    toc: yes
    toc_depth: 4
  html_notebook: 
    fig_caption: yes
    highlight: espresso
    theme: cerulean    
    number_sections: yes
    toc: yes
    toc_depth: 4
  word_document: 
    fig_caption: yes
    highlight: espresso
    theme: cerulean    
    number_sections: yes
    toc: yes
    toc_depth: 4  
  html_document: 
    fig_caption: yes
    highlight: espresso
    theme: cerulean    
    number_sections: yes
    toc: yes
    toc_depth: 4
---  
#1.Introduction  

The data is taken from one of **Kaggle prediction competitions**. The Competition Description can be found [**here**](https://www.kaggle.com/c/titanic). 

**Main features of my EDA**   
- Null value imputation using MICE  
- Feature engineering  
- Predition using Random Forest ML algorithm   

#2.Exploratory Analysis of Data  
##Data Overview
The data has been split into two groups:  

- Training set (train.csv)    
- Test set (test.csv)  
- Results for the test('gender_submission.csv')  

Dataset has 12 variables  

##Data Load
```{r load}

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('stringr') #string operations

setwd("D:/Project/Information Management/Titanic")

#load all the datafiles in different variables
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#result data
gender_submission <- read.csv('gender_submission.csv', header = TRUE)


#join the submission file to get a bigger dataframe
titanic <- rbind(train,merge(test,gender_submission,by='PassengerId'))

#go through all the variables of the file
str(titanic)

```  

##Data reformating
We see most of the labels are miss interpreted by 'R' eg. 'Name' which is showing factor of 1307 levels, which otherwise should be a character. Also in the 'Name' variable we can see that there are only 1307 unique entries instead of 1309.  
```{r data_reformating}
#reformat the datatypes using basic domain knowledge
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Name <- as.character(titanic$Name)
titanic$Cabin <- as.character(titanic$Cabin)
titanic$Ticket <- as.character(titanic$Ticket)

#go through all the variables of the file
str(titanic)
```   

##Investigation
```{r data_investigation}
#no of unique passengers
length(unique(titanic$Name))

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector

dup.names <- titanic[which(duplicated(titanic$Name)), "Name"]


# Next, take a look at the records in the combined data set
titanic[which(titanic$Name %in% dup.names),]
```  

##Feature Engineering
```{r Data_FeatureEngineering}
# Extract title from passenger names
titanic$Title <- gsub('(.*, )|(\\..*)', '', titanic$Name)

# Show title counts by sex
table(titanic$Sex, titanic$Title)

# Titles with very low cell counts to be combined to "Others" level
Others <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also re-assign mlle, ms, and mme accordingly
titanic$Title[titanic$Title == 'Mlle']        <- 'Miss' 
titanic$Title[titanic$Title == 'Ms']          <- 'Miss'
titanic$Title[titanic$Title == 'Mme']         <- 'Mrs' 
titanic$Title[titanic$Title %in% Others]  <- 'Others'


# Family Size feature
titanic$FamilySize <- as.factor(titanic$SibSp + titanic$Parch + 1)
```  

#3.Data Preprocessing

##Missing value Imputation

```{r}
library('mice') # imputation
# We Check the levels and top values when 'str(titanic)' is run and see that cabin and embarked has missing values

#Also we check columns having na values
names(which(sapply(titanic, anyNA)))

#to add Embarkment class we check the class
titanic[titanic$Embarked=='','PassengerId']

#to impute missing values we check corresponding likely values in class and fare
titanic[c(62, 830), c('Embarked','Fare','Pclass')]

#check embarkment againdt fare
embark_fare <- subset(titanic, PassengerId!=62 & PassengerId!=830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = Pclass)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
titanic$Embarked[c(62, 830)] <- 'C'


#Find NA values in Age and Fare
subset(titanic, is.na(Fare))

#This is a third class passenger who has departed value ('S').Visualize Fares among all others sharing their class and embarkment (n = 494).

ggplot(titanic[titanic$Pclass == '3' & titanic$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = 'grey') + 
  geom_vline(aes(xintercept=median(Fare, na.rm=TRUE)),
    colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare value with median fare for class/embarkment
titanic$Fare[1044] <- median(titanic[titanic$Pclass == '3' & titanic$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Show number of missing Age values
sum(is.na(titanic$Age))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(titanic[, !names(titanic) %in% c('PassengerId','Name','Ticket','Cabin','Survived','Fare','Embarked','Title')], method='rf')

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
densityplot(mice_mod)
  
# Replace Age variable from the mice model

titanic$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(titanic $Age))  
```  

#4.Plots based on different factors  
##Survival Based on Class  
```{r data_Analysis}
# Distribution of survival in correlation with classes
table(titanic$Pclass,titanic$Survived)


# Checking Survival based on class
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  ggtitle("Class Based Survival") +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")
```  

##Survival of people based on Age, Gender, Title, Family size  
```{r Gender Based Survival}
# Plot by Gender
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Sex) + 
  ggtitle("Gender Based Survival") +
  xlab("Class") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Show title counts by sex again
table(titanic$Sex, titanic$Title)

# Plot by Title
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Title, ncol=5 ) + 
  ggtitle("Title Based Survival") +
  xlab("Class") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Plot by family size
ggplot(titanic, aes(x = FamilySize, fill = Survived)) +
  geom_bar() + 
  ggtitle("Survival by Family Size") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Plot survival with age and sex  
ggplot(titanic, aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  ggtitle("Survival by Age and Gender") +
  facet_grid(.~Sex) + 
  theme_few()

#Plot survival with age, sex and class
ggplot(titanic, aes(Age, fill = factor(Survived))) + 
  geom_histogram()  + 
  ggtitle("Survival by Age, Class and Gender") +
  facet_wrap(~Sex + Pclass)  +
  theme_few()

```  

#5.Predicting Survival using Random Forest classifier  
```{r Prediction}
library(randomForest) # classification algorithm

#Convert all characters to factors
titanic=titanic %>% mutate_if(is.character, as.factor)

#Check is any missing values
md.pattern(titanic)


titanic=titanic %>% mutate_if(is.character, as.factor)

# Split the data back into a train set and a test set
train <- titanic[1:891,]
test <- titanic[892:1309,]


# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + FamilySize, data = train)

# Show Variable importance
varImpPlot(rf_model)

# Predict using the test set
prediction <- predict(rf_model, test)
table(prediction)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)


# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
```  

#6.Validation  
```{r Data_Validation}
library(caret) #classification and regression training
library(doSNOW) #exploiting parllel execution of the resource

#OOB and Confusion matrix
rf_model

#10 Fold cross validation
set.seed(2348)
Validation <- createMultiFolds(train$Survived, k = 10, times = 10)



# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = Validation)


# Set up doSNOW package for multi-core training. This is helpful as we're going
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

rf.train <- train[, c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]

# Set seed for reproducibility and train
set.seed(34324)
rf_val <- train(x = rf.train, y = train$Survived, method = "rf", tuneLength = 3,ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf_val

```  

This page is created in an R Markdown document or RMD. RMD is an cleaner way of sharing code and documentation.  
*Reference for RMD* - [**udacity**](https://www.youtube.com/watch?v=FORb9Tja-p0&list=PLAwxTw4SYaPlSFCDRaseIGGxlGn81Adjs&index=23)
