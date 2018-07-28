library(data.table)
library(dplyr)      
library(rattle)   

library(caret)
library(DMwR)
library(randomForest)
library(xgboost)
library(kernlab)

#1)load data
fp <- file.path('C:/Users/research/Documents/RStudio/workspace/caret.experiments',
                'twoclass.label.csv')
data <- fread(input = fp, showProgress = FALSE)

data$classLabel <- as.factor(data$classLabel)
data$threatLabel <- as.factor(data$threatLabel)
summary(data$classLabel)

#parameters to tune:
##method
##perc.over

method.vec  <- c('rf','xgbLinear','svmLinear','svmPoly','svmRadial')
perc.over.vec <- c(200,300,400)

set.seed(42)
getwd()
setwd('C:/Users/research/Documents/RStudio/workspace/caret.experiments')

source('twoclass.smoteminority.R')

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1, outfile='check.txt',type='PSOCK') #not to overload your computer
registerDoParallel(cl)

i<-1
foreach(method= method.vec, .combine='c') %:% 
  foreach(perc.over= perc.over.vec, .combine='c') %dopar%   {
      print(i)
      print(paste(method,perc.over))
      twoclass.smoteminority(data,method,perc.over)
      
      i<-i+1
          }
stopCluster(cl)

