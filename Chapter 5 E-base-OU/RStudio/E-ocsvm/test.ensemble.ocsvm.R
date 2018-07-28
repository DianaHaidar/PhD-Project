library(data.table)
library(dplyr)   
library(caret)
library(amap)
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

e.size.vec <- c(2,3,4)
kmeans.method.vec <- c('euclidean')
kernel.vec <- c('linear','polynomial','radial')

set.seed(42)
getwd()
setwd('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test')

source('ensemble.ocsvm.R')
for(e.size in e.size.vec)
  for(kmeans.method in kmeans.method.vec)
    for(kernel in kernel.vec)
    {
      print(paste(e.size,kernel,sep=','))
      ensemble.ocsvm(data,e.size,kmeans.method,kernel)
    }

