ocsvm <- function(data,kernel) {
  
  main.dir <- 'C:/Users/ID916780/Documents/RStudio/workspace/oneclass.test'
  sub.dir <- paste(kernel,sep='')
  dir.create(file.path(main.dir, sub.dir))

majority.data <- data[data$classLabel=='Normal']
summary(majority.data$classLabel)
minority.data <- data[data$classLabel=='Anomalous']
summary(minority.data$classLabel)

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

  set.seed(42)
  svm.model<-svm(train.data[,1:16],y=NULL,
                 type='one-classification',
                 nu=0.5,
                 scale=TRUE,
                 kernel=kernel)
  
  svm.pred.boolean <- predict(svm.model,test.data[,1:16])
  svm.pred <- ifelse(svm.pred.boolean == TRUE,'Normal','Anomalous')
  
  predict.fold.file <- paste('fold',f.test.num,'.predict.data.class.label.csv',sep='')
  write.csv(x=svm.pred,row.names = FALSE,
            file = paste(main.dir,sub.dir,predict.fold.file,sep='/'))
  cm.table <- table(svm.pred, test.data.label)
  TP <- TP + cm.table[1]
  FP <- FP + cm.table[3]
  FN <- FN + cm.table[2]
  TN <- TN + cm.table[4]


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
