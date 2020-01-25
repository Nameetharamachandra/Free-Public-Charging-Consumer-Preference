---
  title: "Free Charging Trial Survey"
author: "Michael Maness"
date: "November 18, 2019"
output:
  html_document: default
pdf_document: default
---
  
#{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(apollo)
library(ggplot2)
#

## Load Dataset


#{r load-data}
data.wide <- read.csv("C:\\Users\Namee\OneDrive\Desktop\EV_Project\Trial Surveytrial_data_freecharge_coded.csv")
data.wide$ID <- 1:nrow(data.wide)
design <- read.csv('experimental_design_trial.csv')
#

## Table Manipulation


#{r data-long}
data <- data.wide %>% pivot_longer(starts_with('Scenario'),
                                   names_to = c('ScenarioNum'),
                                   names_pattern = 'Scenario(.*)',
                                   values_to = 'Choice')




#{r data-elim-scenarios}
data <- data %>% filter(!is.na(Choice))



  
#{r combine-data-design}
colnames(design)[1] <- 'ScenarioNum'
data$ScenarioNum <- as.integer(data$ScenarioNum)
data <- left_join(data,design, by='ScenarioNum')
write.csv(data,file = "EVData.csv",row.names = FALSE)

##Data Cleaning
#remove unwanted columns
data$Birthyear<- ifelse(data$Birthyear>1900, 2020-data$Birthyear,data$Birthyear)
data<-data[-c(1:30,32:55,57:60,78,81,89:90)]

#To remove all the rows with NA values
data<-data[1:43]
data[data[1:43] == -99] <- NA
data<-na.omit(data)
data$ScenarioNum<-NULL

#function to check the factor levels in columns
factor_level<-function(column_name){
  tbl<-with(data, table(column_name))
  return(tbl)
}
summary(data)
#data$Birthyear<- ifelse(data$Birthyear>1900, 2020-data$Birthyear,data$Birthyear)
#Model
install.packages('rpart')
install.packages('rattle')

# load libraries
library(rpart)
library(rattle)
rpart <- rpart(Choice ~ ., data=data, method="class")
install.packages("randomForest")
library(randomForest)
# plot decision tree
fancyRpartPlot(rpart, main="ev")
set.seed(123) #randomization`

#creating indices
library(caret)
library(randomForest)
trainIndex <- createDataPartition(data$Choice,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
TRAIN <- data[trainIndex,] #training data (75% of data)

TEST <- data[-trainIndex,] #testing data (25% of data)
r <- randomForest(Choice ~ ., data=TRAIN, importance=TRUE, do.trace=100)
table(TRAIN$Choice)
r
table(r$predicted==TRAIN$Choice)
r$confusion
rf_validation <- predict(r, TEST, type="class")
test_factor<-``
confusionMatrix(rf_validation, TEST$Choice)
