library(data.table)
library(dplyr)   
library(caret)
library(isofor)

#1)load data
fp <- file.path('C:/Users/fatima/Documents/RStudio/workspace/oneclass.test',
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
nt <- 20
phi.param.vec  <- c(0)
phi.param <- 0
ascore.thresh.vec  <- c(0.35,0.4,0.45,0.5)
FP.chunk.size.vec <- c(10,20,40,60,80,100)

# nt  <- 100
# phi  <- 300
# ascore.thresh  <- 0.3

set.seed(42)
getwd()
setwd('C:/Users/fatima/Documents/RStudio/workspace/oneclass.test')

source('base.iForest.update.FP.chunk.R')

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
i<-1
foreach(ascore.thresh= ascore.thresh.vec, .combine=cbind) %:%
  foreach(FP.chunk.size= FP.chunk.size.vec, .combine='c') %do% {
    
    print(i)
    print(paste(nt,phi.param,ascore.thresh,FP.chunk.size,sep=','))
    base.iForest.update.FP.chunk(data,nt, phi.param, ascore.thresh,FP.chunk.size)
    i<-i+1
  }
#stop cluster
stopCluster(cl)

# for(nt in nt.vec)
#   for(phi in phi.vec)
#     for(ascore.thresh in ascore.thresh.vec)
#       for(FP.chunk.size in FP.chunk.size.vec)
#       {
#           print(paste(nt,phi,ascore.thresh,FP.chunk.size,sep=','))
#           base.iForest.update.FP.chunk(data,nt, phi, ascore.thresh,FP.chunk.size)
#       }
# 
