ensemble.ocsvm.update.FP.chunk <- function(data,e.size,kmeans.method,kernel,FP.chunk.size) {
  
  main.dir <- 'C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test'
  sub.dir <- paste(e.size,kernel,FP.chunk.size,sep=',')
  dir.create(file.path(main.dir, sub.dir))

majority.data <- data[data$classLabel=='Normal']
summary(majority.data$classLabel)
minority.data <- data[data$classLabel=='Anomalous']
summary(minority.data$classLabel)

#2)decompose majority class data (all data)
names(majority.data)

set.seed(42)  
majority.clusters <- Kmeans(majority.data[,1:16], e.size, nstart = 20, method=kmeans.method)
#majority.clusters <- kmeans(majority.data[,1:16], e.size , nstart = 20)
majority.clusters$cluster <- as.factor(majority.clusters$cluster)

#assign cluster label instead of class label for majority data
majority.data
majority.clusters$cluster
majority.cluster.label <- paste('Cluster',majority.clusters$cluster,sep='')
majority.data[,'classLabel'] <- majority.cluster.label
majority.data$classLabel <- as.factor(majority.data$classLabel)
majority.data

#4)partition decompose.majority.data to train 90% and test 10%
folds.indices <- createFolds(majority.data$classLabel,k = 10,list = TRUE,returnTrain = FALSE)
folds.data <- lapply(folds.indices, function(ind, dat) dat[ind,], dat=majority.data)
#unlist(lapply(splitup, nrow))
TP.avg <- 0
FP.avg <- 0
FN.avg <- 0
TN.avg <- 0

TPt.avg <- 0

f.test.num <- 1
for(f.test in folds.data)
{
  #reinitialize FP mainly
  TP <- 0
  FP <- 0
  FN <- 0
  TN <- 0
  
  TPt <- 0
  
  majority.test.data <- f.test
  test.data <- rbind(majority.test.data,minority.data)
  summary(test.data$classLabel)
  dim.test.data <- dim(test.data)
  dimT <- dim.test.data[1]
  test.data
  
  #shuffle test.data: randomize the order of data frame 
  test.data <- test.data[sample(1:nrow(test.data)), ]
  test.data
  
  train.data <- NULL
  f.train.num <- 1
  for(f.train in folds.data)
  {
    if(f.train.num != f.test.num)
      train.data <- rbind(train.data,f.train)
    f.train.num <- f.train.num +1    
  }
  summary(train.data$classLabel)
  
  test.data.threat.label <- as.matrix(test.data[,18])
  test.fold.file <- paste('fold',f.test.num,'.test.data.threat.label.csv',sep='')
  write.csv(x=test.data.threat.label,row.names = FALSE,
            file = paste(main.dir,sub.dir,test.fold.file,sep='/'))
  test.data.label <- as.matrix(test.data[,17])
  train.data <- train.data[,1:17]
  test.data <- test.data[,1:17]
  
 
  #6)construct an ensemble with e.size svm base classifiers 
  svm.pred.bool <- array(FALSE,c(dimT,e.size)) 
  cluster <- rep(list(NULL),e.size)
  svm.model <- rep(list(NULL),e.size)
  
  for(e in 1:e.size)
  {
    
    c.label <- paste("Cluster",e, sep='')
    cluster[[e]] <- train.data[train.data$classLabel == c.label,]
    cluster[[e]]
    
    #offline train on train data
    svm.model[[e]]<-svm(cluster[[e]][,1:16],y=NULL,
                   type='one-classification',
                   nu=0.5,
                   scale=FALSE,
                   kernel=kernel)
    
  } 
  ######################################################
  #step1: test instance after instance
  svm.pred <- array('',c(dimT,1))
  
  for(inst in 1:dimT)
  {
    
    test.instance <- test.data[inst,]
    #calculate svm.pred.bool for each test instance
    #save for vote
    for(e in 1:e.size)
    {
      svm.pred.bool[inst,e] <- predict(svm.model[[e]],test.instance[1,1:16])
    }
    
    svm.pred[inst, ] <- ifelse(any(svm.pred.bool[inst,] == TRUE),'Normal','Anomalous')
    test.instance.label <- as.matrix(test.instance[1,17])
    
    if(test.instance.label=='Anomalous' && svm.pred[inst, ] =='Anomalous')
      TP <- TP + 1
    #no 'Normal' class label in ensemble
    if(test.instance.label!='Anomalous' && svm.pred[inst, ] =='Anomalous')
      FP <- FP + 1
    if(test.instance.label=='Anomalous' && svm.pred[inst, ] =='Normal')
      FN <- FN + 1
    if(test.instance.label!='Anomalous' && svm.pred[inst, ] =='Normal')
      TN <- TN + 1
    
    #step2: append cluster to test.instance.FP
    if(test.instance.label!='Anomalous' && svm.pred[inst, ]=='Anomalous')
    {
      for(e in 1:e.size)
      {
        
        cluster[[e]] <- rbind(cluster[[e]],test.instance)
        #if(FP%%FP.chunk.size ==0)
        if((FP!=0) && (FP%%FP.chunk.size ==0))
        {
          #step3: update (retrain) ocsvm model
          set.seed(42)
          svm.model[[e]]<-svm(cluster[[e]][,1:16],y=NULL,
                              type='one-classification',
                              nu=0.5,
                              scale=TRUE,
                              kernel=kernel)
        }
      }
    }
    
    
  }
  ######################################################
  
  predict.fold.file <- paste('fold',f.test.num,'.predict.data.class.label.csv',sep='')
  write.csv(x=svm.pred,row.names = FALSE,
            file = paste(main.dir,sub.dir,predict.fold.file,sep='/'))
  
  
  threats <- unique(test.data.threat.label)
  threats <- threats[threats!='Normal']
  threats
  
  for(threat in threats)
  {
    threat.indices <-  test.data.threat.label==threat
    threat.indices <- which(threat.indices==TRUE)
    threat.indices
    for(t.index in threat.indices)
    {
      if(svm.pred[t.index]=='Anomalous')
      {
        TPt <- TPt+1
        break
      }
    }
  }
  
  TP.avg <- TP.avg + TP
  FP.avg <- FP.avg + FP
  FN.avg <- FN.avg + FN
  TN.avg <- TN.avg + TN
  
  TPt.avg <- TPt.avg + TPt
  
  f.test.num <- f.test.num +1
  
}
TP.avg <- ceiling(TP.avg/10)
FP.avg <- ceiling(FP.avg/10)
FN.avg <- ceiling(FN.avg/10)
TN.avg <- ceiling(TN.avg/10)

TPt.avg <- ceiling(TPt.avg/10)

measures <- c(TP.avg,FP.avg,FN.avg,TN.avg,TPt.avg)
write.csv(x=measures,row.names = FALSE,
          file = paste(main.dir,sub.dir,'measures.csv',sep='/'))
}