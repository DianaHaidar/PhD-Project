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

e.size.vec <- c(2,4)
kmeans.method.vec <- c('euclidean')
kernel.vec <- c('linear','polynomial','radial')
FP.chunk.size.vec <- c(20,40,60,80,100)

set.seed(42)
getwd()
setwd('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test')

source('ensemble.ocsvm.update.FP.chunk.R')
for(e.size in e.size.vec)
  for(kmeans.method in kmeans.method.vec)
    for(kernel in kernel.vec)
      for(FP.chunk.size in FP.chunk.size.vec)
    {
      print(paste(e.size,kernel,FP.chunk.size,sep=','))
        ensemble.ocsvm.update.FP.chunk(data,e.size,kmeans.method,kernel,FP.chunk.size) 
    }

