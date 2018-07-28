library(data.table)
library(dplyr)   
library(caret)
library(isofor)

#1)load data
fp <- file.path('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test',
                'twoclass.label.csv')
data <- fread(input = fp, showProgress = FALSE)

data$classLabel <- as.factor(data$classLabel)
data$threatLabel <- as.factor(data$threatLabel)
summary(data$classLabel)

#parameters to tune:
##nt : the number of trees in the ensemble
##phi : the number of samples to draw without replacement to construct each tree
##ascore.thresh : controls the threshold of an anomaly score to predict an instance as anomalous

# ascore.thresh.vec  <- c(0.7,0.8,0.9)

nt.vec  <- c(20)
phi.param.vec  <- c(0)
ascore.thresh.vec  <- c(0.35,0.4,0.45,0.5)

# nt  <- 100
# phi  <- 300
# ascore.thresh  <- 0.3

set.seed(42)
getwd()
setwd('C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test')

source('base.iForest.R')
for(nt in nt.vec)
  for(phi.param in phi.param.vec)
    for(ascore.thresh in ascore.thresh.vec)
    {
      print(paste(nt,phi.param,ascore.thresh,sep=','))
      base.iForest(data,nt, phi.param, ascore.thresh)
    }

