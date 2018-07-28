ensemble.ocsvm <- function(data,e.size,kmeans.method,kernel) {
  
  main.dir <- 'C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test'
  sub.dir <- paste(e.size,kernel,sep=',')
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

TP <- 0
FP <- 0
FN <- 0
TN <- 0

TPt <- 0

f.test.num <- 1
for(f.test in folds.data)
{
  
  majority.test.data <- f.test
  test.data <- rbind(majority.test.data,minority.data)
  summary(test.data$classLabel)
  
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
  test.dim <- dim(test.data)
  
  #6)construct an ensemble with e.size svm base classifiers 
  svm.pred.bool <- array(FALSE,c(test.dim[1],e.size)) 
  for(e in 1:e.size)
  {
    c.label <- paste("Cluster",e, sep='')
    cluster <- train.data[train.data$classLabel == c.label,]
    
    svm.model<-svm(cluster[,1:16],y=NULL,
                   type='one-classification',
                   nu=0.5,
                   scale=FALSE,
                   kernel=kernel)
    
    #calculate svm.pred.bool for each test instance
    svm.pred.bool[,e] <- predict(svm.model,test.data[,1:16])
  }
  
  #7)find the anomalous instances
  svm.pred <- array('',c(test.dim[1],1))
  for(inst in 1:test.dim[1])
  {
    #if at least one classifier votes TRUE then it is Normal
    #if all vote FALSE, then it is Anomalous
    svm.pred[inst, ] <- ifelse(any(svm.pred.bool[inst,] == TRUE),'Normal','Anomalous')
  }
  svm.pred[svm.pred=='Anomalous']
  
  predict.fold.file <- paste('fold',f.test.num,'.predict.data.class.label.csv',sep='')
  write.csv(x=svm.pred,row.names = FALSE,
            file = paste(main.dir,sub.dir,predict.fold.file,sep='/'))
  cm.table <- table(svm.pred, test.data.label)
  cm.table
  TP <- TP + cm.table[1]
  
  FN <- FN + cm.table[2]
  
  ifelse(e.size == 2, FP <- FP + cm.table[3]+cm.table[5],
         ifelse(e.size == 4, FP <- FP + cm.table[3]+cm.table[5]
                + cm.table[7]+cm.table[9],
                FP <- FP + cm.table[3]+cm.table[5]
                + cm.table[7]+cm.table[9]
                + cm.table[11]+cm.table[13]))
  
  ifelse(e.size == 2, TN <- TN + cm.table[4]+cm.table[6],
         ifelse(e.size == 4, TN <- TN + cm.table[4]+cm.table[6]
                + cm.table[8]+cm.table[10],
                TN <- TN + cm.table[4]+cm.table[6]
                + cm.table[8]+cm.table[10]
                + cm.table[12]+cm.table[14]))
  
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
  f.test.num <- f.test.num +1
}
TP <- ceiling(TP/10)
FP <- ceiling(FP/10)
FN <- ceiling(FN/10)
TN <- ceiling(TN/10)

TPt <- ceiling(TPt/10)

measures <- c(TP,FP,FN,TN,TPt)
write.csv(x=measures,row.names = FALSE,
          file = paste(main.dir,sub.dir,'measures.csv',sep='/'))
}