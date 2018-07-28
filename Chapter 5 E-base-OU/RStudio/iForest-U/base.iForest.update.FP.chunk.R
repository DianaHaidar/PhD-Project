base.iForest.update.FP.chunk <- function(data,nt,phi.param,ascore.thresh,FP.chunk.size) {
  main.dir <- 'C:/Users/fatima/Documents/RStudio/workspace/oneclass.test'
  sub.dir <- paste(nt,phi.param,ascore.thresh,FP.chunk.size,sep=',')
  dir.create(file.path(main.dir, sub.dir))
  
  majority.data <- data[data$classLabel=='Normal']
  summary(majority.data$classLabel)
  minority.data <- data[data$classLabel=='Anomalous']
  summary(minority.data$classLabel)
  
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
    # write.csv(x=test.data.threat.label,row.names = FALSE,
    #           file = paste(main.dir,sub.dir,test.fold.file,sep='/'))
    test.data.label <- as.matrix(test.data[,17])
    train.data <- train.data[,1:17]
    test.data <- test.data[,1:17]
    
    train.dim <- dim(train.data)
    if(phi.param==0){
      phi <- train.dim[1]
    }else{
      phi <- phi.param
    }
    
    #offline train on train data
    #iForest(X, nt=100, phi=256, seed=1234, multicore=FALSE)
    iForest.model <- iForest(train.data[,1:16], nt=nt, phi=phi, seed=42)
    
    
    ######################################################
    #step1: test instance after instance
    iForest.pred <- NULL
    
    for(inst in 1:dimT)
    {
      test.instance <- test.data[inst,]
      iForest.pred.ascore <- predict(iForest.model,test.instance[1,1:16])
      iForest.pred.instance <- ifelse(iForest.pred.ascore > ascore.thresh,'Anomalous','Normal')
      
      iForest.pred <- c(iForest.pred,iForest.pred.instance)
      iForest.pred
      
      test.instance.label <- as.matrix(test.instance[1,17])
      #cm.table <- table(iForest.pred.instance, test.instance.label)
      if(test.instance.label=='Anomalous' && iForest.pred.instance=='Anomalous')
        TP <- TP + 1
      if(test.instance.label=='Normal' && iForest.pred.instance=='Anomalous')
        FP <- FP + 1
      if(test.instance.label=='Anomalous' && iForest.pred.instance=='Normal')
        FN <- FN + 1
      if(test.instance.label=='Normal' && iForest.pred.instance=='Normal')
        TN <- TN + 1
      
      #step2: append train.data to test.instance.FP
      if(test.instance.label=='Normal' && iForest.pred.instance=='Anomalous')
        train.data <- rbind(train.data,test.instance)
      
      #if(FP%%FP.chunk.size ==0)
       if((FP!=0) && (FP%%FP.chunk.size ==0))
       {
         train.dim <- dim(train.data)
         if(phi.param==0){
           phi <- train.dim[1]
         }else{
           phi <- phi.param
         }
         #step3: update (retrain) iForest model
         set.seed(42)
         iForest.model <- iForest(train.data[,1:16], nt=nt, phi=phi, seed=42)
       }
      
    }
    ######################################################
    
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
        if(iForest.pred[t.index]=='Anomalous')
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
