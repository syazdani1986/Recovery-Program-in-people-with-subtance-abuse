rm (list = ls())
set.seed(1)

getwd() # where I am
setwd("C:/freelancing project/Falling colors project/data and instruction")

#importing Datasets
Crime_varData <- read.csv("Crime_varData.csv", stringsAsFactors = TRUE, header = TRUE)
head(Crime_varData)

Employment_varData <- read.csv("Employment_varData.csv", stringsAsFactors = TRUE, header = TRUE)
head(Employment_varData)


DrugUse_varData <- read.csv("DrugUse_varData.csv", stringsAsFactors = TRUE, header = TRUE)
head(DrugUse_varData)

library(datasets)
library(DAAG)
library(caret)
library(tidyverse)
library(mlbench)


#regression model for Commited Crime
#using cross validation cv.lm function with our previous linear model


lm_Crime_varData<- lm(E4CommitedCrime~. , data=Crime_varData)

data_ctrl <- trainControl(method = "cv", number = 10,verboseIter = TRUE)
CVModel_Crime_varData <- train(E4CommitedCrime~. ,   
                     data = Crime_varData,                        
                     trControl = data_ctrl,             
                     method = "lm",                      
                     na.action = na.pass)  

summary(CVModel_Crime_varData)
CVModel_Crime_varData
summary(lm_Crime_varData)

AIC(lm_Crime_varData)


#regresssion model for emplymentSituation
#using cross validation cv.lm function with our previous linear model


lm_Employment_varData<- lm(D3EmploymentSituationId~. , data=Employment_varData)

data_ctrl1 <- trainControl(method = "cv", number = 10,verboseIter = TRUE)
CVmodel_Employment_varData <- train(D3EmploymentSituationId~. ,   
                     data = Employment_varData,                        
                     trControl = data_ctrl1,             
                     method = "lm",                      
                     na.action = na.pass)  

summary(CVmodel_Employment_varData)

summary(lm_Employment_varData)

AIC(lm_Employment_varData)

#regresssion model for DrugUse
#using cross validation cv.lm function with our previous linear model


lm_DrugUse_varData<- lm(B1cIllegalDrugs~. , data=DrugUse_varData)

data_ctrl2 <- trainControl(method = "cv", number = 10,verboseIter = TRUE)
CVmodel_DrugUse_varData <- train(B1cIllegalDrugs~. ,   
                      data = DrugUse_varData,                        
                      trControl = data_ctrl2,             
                      method = "lm",                      
                      na.action = na.pass)  

summary(CVmodel_DrugUse_varData)

summary(lm_DrugUse_varData)

AIC(lm_DrugUse_varData)

set.seed(42)


# 5 FOLD Cross validation  Random forest model 



# Random forest model for Drug use in past 30 days variable

data_ctrl3 <- trainControl(method = "cv", number = 5,
                           search = 'random', savePredictions = T)
model_fitRF_DrugUse <- train(B1cIllegalDrugs~. ,   
                      data = DrugUse_varData,                        
                      trControl = data_ctrl3,             
                      method = "rf",                      
                      tunelLength = 7,
                     ntree = 200,
                     na.action = na.pass)  

model_fitRF$bestTune

model_fitRF_DrugUse


# Random forest model for Crime commited in past 30 days variable
data_ctrl4 <- trainControl(method = "cv", number = 5,
                           search = 'random', savePredictions = T)
model_fitRF_CrimeCommited <- train(E4CommitedCrime~. ,   
                             data = Crime_varData,                        
                             trControl = data_ctrl4,             
                             method = "rf",                      
                             tunelLength = 7,
                             ntree = 200,
                             na.action = na.pass)  

model_fitRF_CrimeCommited

# Random forest model for employment siuation in past 30 days variable

data_ctrl5 <- trainControl(method = "cv", number = 5,
                           search = 'random', savePredictions = T)
model_fitRF_EmploymentSituation <- train(D3EmploymentSituationId ,   
                             data = Employment_varData,                        
                             trControl = data_ctrl5,             
                             method = "rf",                      
                             tunelLength = 7,
                             ntree = 200,
                             na.action = na.pass)  
