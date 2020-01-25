data.wide <- read.csv('trial_data_freecharge_coded.csv')
data.wide$ID <- 1:nrow(data.wide)

#-------------------Gender-----------------------------------------------------------
tbl <- with(data.wide, table(Gender))
barplot(tbl,main="Respondent Gender",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("blue","red"),las=2,width=c(0.01,0.01),space=3,
       names.arg = c("Male","Female"),ylab="Respondents",xlab="Gender")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))

#--------------------------age---------------------------------------------

data.wide$Birthyear<- ifelse(data.wide$Birthyear>1900, 2020-data.wide$Birthyear,data.wide$Birthyear)
tbl<-table(cut(data.wide$Birthyear, breaks=seq(18, 99, 10)))
barplot(tbl,main="Respondent Age", beside = TRUE,ylim=c(0L,60),col=c("darkblue"),
        ,names.arg = c("18-28","28-38","38-48","48-58","58-68","68-78","78-88","88-98"),xlab="Age(Years)",ylab="Respondents")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
summary(data.wide$Birthyear)
#------------------------------Education---------------
tbl <- with(data.wide, table(Edu))
(cbind(tbl,prop.table(tbl) *100))
tbl
#--------------Driver-------------------------------------
data.wide$
tbl <- with(data.wide, table(Driver))
barplot(tbl,main="Respondent Driving Status",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("black","darkblue","red"),width=c(0.01,0.01),space=1,
        legend=c("NA","Yes","NO"),names.arg = c("NA","YES","NO"),ylab="Respondents",xlab="Driving Status")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
#-----------------------------FamiliarEV--------------------------
install.packages("RColorBrewer")
library(RColorBrewer)
coul <- brewer.pal(4, "Set2")
tbl <- with(data.wide, table(FamilarEV))
barplot(tbl,main="Respondent familiarity with EVs",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("black","darkblue","red","grey"),width=c(0.01,0.01),space=1,
        names.arg = c("Very","Moderate","Slight","Not Familiar"),ylab="Respondents",xlab="Familiarity with EV")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
#----------------Household----------------------
data.wide$Household.Income
tbl <- with(data.wide, table(Household.Income))
barplot(tbl,main="Respondent HouseHold Income",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=1,las=2,
        ,cex.names=0.65,names.arg = c("NA","<15k","15k-25k","25k-34k","35k-50k","50k-75k","75k-100k","100k-150k","200k-250k",">250k"),ylab="Respondents",xlab="Household Income(dollars)")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
#-------------------------Household member number---------
data.wide$Household.MemberNo
coul <- brewer.pal(9, "Set1")
tbl <- with(data.wide, table(Household.MemberNo))
barplot(tbl,main="Respondent Total HouseHold Members",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=0.5,
       names.arg = c("NA","1","2","3","4","5","6","7","8"),ylab="Respondents",xlab="Total Household Members")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
#-----------------------------Home Type---------------------------
data.wide$Home.Type

coul <- brewer.pal(7, "Set2")
tbl <- with(data.wide, table(Home.Type))
barplot(tbl,main="Respondent Home Type",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=1,cex.names=.75,
        names.arg = c("Detached","Townhouse","Apartment","Condo","MobileHome","Dorm","Other"),ylab="Respondents",xlab="Home Type")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
#-----------------------urban/Rural---------------------------
data.wide$Urban.Rural

coul <- brewer.pal(3, "Set2")
tbl <- with(data.wide, table(Urban.Rural))
barplot(tbl,main="Respondent House Area",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("black","darkblue","red"),width=c(0.01,0.01),space=0.5,cex.names=.75,
        names.arg = c("Urban","Surban ","Rural"),ylab="Respondents",xlab="Area")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
#------------------------Number of Drivers------------------------------------------------
data.wide$Number.of.Drivers

coul <- brewer.pal(5, "Set2")
tbl <- with(data.wide, table(Number.of.Drivers))
barplot(tbl,main="Respondent Household No.of.Drivers",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=2,cex.names=.75,
        names.arg = c("NA","2","3","4","5"),ylab="Respondents",xlab="Drivers")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
#---------------------------Number of Vehicles-----------------
data.wide$Number.of.Vehicles

coul <- brewer.pal(5, "Set2")
tbl <- with(data.wide, table(Number.of.Vehicles))
barplot(tbl,main="Respondent Household No.of.Vehicles",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=0.5,cex.names=.75,
        names.arg = c("1","2","3","4","6"),ylab="Respondents",xlab="Vehicles")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
#------------Travel Time----------------
summary(data.wide$Travel.Time)
coul<-rewer.pal(5, "Set2")
tbl<-table(cut(data.wide$Travel.Time, breaks=seq(0, 100, 20)))
barplot(tbl,main="Respondent Commute Time",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=0.5,cex.names=.75,
        names.arg = c("0-20","20-40","40-60","60-80","80-100"),ylab="Respondents",xlab="Commute Time")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
tbl
#---------------------------------worker no-----------------
tbl<-table(cut(data.wide$Travel.Time, breaks=seq(0, 100, 20)))
barplot(tbl,main="Respondent Travel Time from work",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=0.5,cex.names=.75,
        names.arg = c("0-20","20-40","40-60","60-80","80-100"),ylab="Respondents",xlab="TravelTime")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
tbl
tbl
#----------------------------------age1------------------
data.wide$Household.Member.Age_1
tbl<-with(data.wide, table(Household.Member.Age_10))
tbl
#--------------------------workers------------------------
data.wide$WorkerNo
tbl<-with(data.wide, table(WorkerNo))
barplot(tbl,main="No of workers in Respondent's HouseHold",beside = TRUE,ylim=c(0L,nrow(data.wide)),col=c("darkblue"),width=c(0.01,0.01),space=0.5,cex.names=.75,
        names.arg = c("NA","1","1","3","4","5","6","7"),ylab="Respondents",xlab="Number of workers")
prop.table(tbl)
(cbind(tbl,prop.table(tbl) *100))
#--------------------------------------Scenario Based-------------------------
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
data.wide <- read.csv('trial_data_freecharge_coded.csv')
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

##Data Cleaning
#remove unwanted columns
data<-data[-c(1:30,32:55,57:60,78,81,89:90)]

#To remove all the rows with NA values
data<-data[1:43]
data[data[1:43] ==-99] <- NA
data<-na.omit(data)
data$Birthyear<- ifelse(data$Birthyear>1900, 2020-data$Birthyear,data$Birthyear)
data$ScenarioNum<-NULL
#function to check the factor levels in columns
factor_level<-function(column_name){
  tbl<-with(data, table(column_name))
  return(tbl) 
}
summary(data)

#Model
install.packages('rpart')
install.packages('rattle')

# load libraries
library(rpart)
library(rattle)
rpart <- rpart(Choice ~ ., data=data, method="class")

fancyRpartPlot(rpart, main="ev")
