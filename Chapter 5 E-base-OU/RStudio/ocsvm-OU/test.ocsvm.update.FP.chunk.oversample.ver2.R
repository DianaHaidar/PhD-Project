library(data.table)
library(dplyr)   
library(caret)
library(e1071)
library(Rlof)
library(DMwR2)

#1)load data
fp <- file.path('C:/Users/research/Documents/RStudio/workspace/oneclass.test',
                'twoclass.label.csv')
data <- fread(input = fp, showProgress = FALSE)

data$classLabel <- as.factor(data$classLabel)
data$threatLabel <- as.factor(data$threatLabel)
summary(data$classLabel)

#parameters to tune:
##kernel: svm kernel

# ascore.thresh.vec  <- c(0.7,0.8,0.9)

kernel.vec <- c('linear','polynomial','radial')
FP.chunk.size.vec <- c(20,40,60,80,100)

set.seed(42)
getwd()
setwd('C:/Users/research/Documents/RStudio/workspace/oneclass.test')

source('ocsvm.update.FP.chunk.oversample.ver2.R')
perc.over <- 200

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1, outfile='check.txt',type='PSOCK') #not to overload your computer
registerDoParallel(cl)
#clusterExport(cl, 'data')
#clusterEvalQ(cl, library(rms))
i<-1
foreach(kernel= kernel.vec, .combine=cbind) %:%
  foreach(FP.chunk.size= FP.chunk.size.vec, .combine='c') %dopar%   {
    
    print(i)
    print(paste(kernel,FP.chunk.size,perc.over,sep=','))
    ocsvm.update.FP.chunk.oversample(data,kernel,FP.chunk.size,perc.over)
    
    i<-i+1
  }
#stop cluster
stopCluster(cl)

# for(kernel in kernel.vec)
#   for(FP.chunk.size in FP.chunk.size.vec)
#     {
#       print(paste(kernel,FP.chunk.size,perc.over,sep=','))
#       ocsvm.update.FP.chunk.oversample(data,kernel,FP.chunk.size,perc.over)
#     }

