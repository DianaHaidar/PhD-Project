library(data.table)
library(dplyr)   
library(caret)
library(Rlof)
library(DMwR2)
library(amap)
library(isofor)

#1)load data
fp <- file.path('C:/Users/research/Documents/RStudio/workspace/oneclass.test',
                'twoclass.label.csv')
data <- fread(input = fp, showProgress = FALSE)

data$classLabel <- as.factor(data$classLabel)
data$threatLabel <- as.factor(data$threatLabel)
summary(data$classLabel)

#parameters to tune:
e.size<-2
kmeans.method <- 'euclidean' 
nt <- 20
phi.param <- 0
ascore.thresh.vec  <- c(0.35,0.4,0.45,0.5)
FP.chunk.size.vec <- c(20,40,60,80,100)
perc.over <-200

set.seed(42)
getwd()
setwd('C:/Users/research/Documents/RStudio/workspace/oneclass.test')

source('ensemble.iForest.update.FP.chunk.oversample.ver2.R')

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1, outfile='check.txt',type='PSOCK') #not to overload your computer
registerDoParallel(cl)
#clusterExport(cl, 'data')
#clusterEvalQ(cl, library(rms))
i<-1
foreach(ascore.thresh= ascore.thresh.vec, .combine=cbind) %:%
  foreach(FP.chunk.size= FP.chunk.size.vec, .combine='c') %dopar%   {
    
    print(i)
    print(paste(e.size,kmeans.method, nt,phi.param,ascore.thresh,FP.chunk.size,perc.over,sep=','))
    ensemble.iForest.update.FP.chunk.oversample(data,e.size,kmeans.method, nt,phi.param,ascore.thresh,FP.chunk.size,perc.over) 
    
    
    i<-i+1
  }
#stop cluster
stopCluster(cl)

