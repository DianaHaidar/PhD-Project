library(data.table)
library(dplyr)   
library(caret)
library(amap)
library(isofor)

#1)load data
fp <- file.path('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test',
                'twoclass.label.csv')
data <- fread(input = fp, showProgress = FALSE)

data$classLabel <- as.factor(data$classLabel)
data$threatLabel <- as.factor(data$threatLabel)
summary(data$classLabel)

#parameters to tune:
##e.size : ensemble size
##nt : the number of trees in the ensemble
##phi : the number of samples to draw without replacement to construct each tree
##ascore.thresh : controls the threshold of an anomaly score to predict an instance as anomalous
# e.size.vec  <- c(2,3,4)
# nt.vec  <- c(50,100,150)
# phi.vec  <- c(100,200,300)
# ascore.thresh.vec  <- c(0.7,0.8,0.9)

e.size.vec  <- c(2,4)
kmeans.method.vec <- c('euclidean')
#, 'binary', 'correlation'
nt.vec  <- c(20)
phi.param.vec  <- c(0)
ascore.thresh.vec  <- c(0.35,0.4,0.45,0.5)

set.seed(42)
getwd()
setwd('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test')

source('ensemble.iForest.R')
for(e.size in e.size.vec)
  for(kmeans.method in kmeans.method.vec)
    for(nt in nt.vec)
      for(phi.param in phi.param.vec)
        for(ascore.thresh in ascore.thresh.vec)
        {
          print(paste(e.size,kmeans.method,nt,phi.param,ascore.thresh,sep=','))
          ensemble.iForest(data,e.size,kmeans.method,nt,phi.param,ascore.thresh)
        }

