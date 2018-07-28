library(data.table)
library(dplyr)   
library(caret)
library(e1071)

#1)load data
fp <- file.path('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test',
                'twoclass.label.csv')
data <- fread(input = fp, showProgress = FALSE)

data$classLabel <- as.factor(data$classLabel)
data$threatLabel <- as.factor(data$threatLabel)
summary(data$classLabel)

#parameters to tune:
##kernel: svm kernel

# ascore.thresh.vec  <- c(0.7,0.8,0.9)

kernel.vec <- c('linear','polynomial','radial')

set.seed(42)
getwd()
setwd('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test')

source('ocsvm.R')

for(kernel in kernel.vec)
{
  print(kernel)
  ocsvm(data,kernel)
}

