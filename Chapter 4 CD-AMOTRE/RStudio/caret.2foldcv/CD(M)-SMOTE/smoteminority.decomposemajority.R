smoteminority.decomposemajority <- function(data,method,perc.over) {
  
  main.dir <- 'C:/Users/research/Documents/RStudio/workspace/caret.experiments'
  sub.dir <- paste(method,perc.over)
  dir.create(file.path(main.dir, sub.dir))
  
  library(data.table)
  library(dplyr)      
  library(rattle)   
  
  library(caret)
  library(DMwR)
  library(randomForest)
  library(xgboost)
  library(kernlab)
  
  ###DECOMPOSE MAJORITY
  #save minority.data
  minority.data <- data[data$classLabel=="Anomalous",]
  minority.data$classLabel <- factor(minority.data$classLabel)
  #get majority.data (to decompose)
  majority.data <- data[data$classLabel=="Normal",]
  majority.data$classLabel <- factor(majority.data$classLabel)
  #decompose
  set.seed(20)
  majority.clusters <- kmeans(majority.data[,1:16], 2, nstart = 20)
  majority.clusters$cluster <- as.factor(majority.clusters$cluster)
  clusterLabel <- sub("^", "nCluster", majority.clusters$cluster )
  majority.data$classLabel <- clusterLabel
  majority.data$classLabel <- as.factor(majority.data$classLabel)
  #append minority.append.majority.data
  minority.append.majority.data <- rbind(minority.data,majority.data)
  data <- minority.append.majority.data
  ###
  
  folds.indices <- createFolds(data$classLabel,k = 2,list = TRUE,returnTrain = FALSE)
  folds.data <- lapply(folds.indices, function(ind, dat) dat[ind,], dat=data)
  
  TP <- 0
  FP <- 0
  FN <- 0
  TN <- 0
  
  TPt <- 0
  Pt <- 0
  
  f.test.num <- 1
  for(f.test in folds.data)
  {
    
    test.data <- f.test
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
    
    ###SMOTE
    #save majority.train (to decompose/not to smote)
    majority.train.data <- train.data[train.data$classLabel!="Anomalous",]
    #apply smote on train.data
    set.seed(42)
    smote.train.data <- SMOTE(classLabel ~., train.data[,1:17],perc.over=perc.over)
    #extract smoted minority.train.data (original+artificial)
    smote.minority.train.data <- smote.train.data[smote.train.data$classLabel=="Anomalous",]
    #append smote.minority.train + majority.train
    smote.minority.train.append.majority.train.data <- rbind(smote.minority.train.data, majority.train.data,fill=TRUE)
    train.data <- smote.minority.train.append.majority.train.data
    ###
    
    test.data.threat.label <- as.matrix(test.data[,18])
    test.data.label <- as.matrix(test.data[,17])
    train.data <- train.data[,1:17]
    test.data <- test.data[,1:17]
    
    ctrl <- trainControl(method = "repeatedcv",
                         number = 2,
                         repeats = 2,
                         verboseIter = FALSE)
    
    set.seed(42)
    m.model <- caret::train(classLabel ~.,
                            data = train.data,
                            method = method,
                            trControl = ctrl,
                            scale=FALSE)
    
    m.predict <- predict(m.model, test.data)
    cm <- confusionMatrix(m.predict, test.data$classLabel)
    
    cm.table <- table(m.predict, test.data.label)
    TP <- TP + cm.table[1] 
    FP <- FP + cm.table[4] + cm.table[7] 
    FN <- FN + cm.table[2] + cm.table[3] 
    TN <- TN + cm.table[5] + cm.table[6] + cm.table[8] + cm.table[9]
    
    
    threats <- unique(test.data.threat.label)
    threats <- threats[threats!='Normal']
    threats
    threats
    threats.dim <- length(threats) 
    Pt <- Pt + as.numeric(threats.dim)
    
    for(threat in threats)
    {
      threat.indices <-  test.data.threat.label==threat
      threat.indices <- which(threat.indices==TRUE)
      threat.indices
      for(t.index in threat.indices)
      {
        if(m.predict[t.index]=='Anomalous')
        {
          TPt <- TPt+1
          break
        }
      }
    }
    f.test.num <- f.test.num +1
  }
  TP <- ceiling(TP/2)
  FP <- ceiling(FP/2)
  FN <- ceiling(FN/2)
  TN <- ceiling(TN/2)
  
  TPt <- ceiling(TPt/2)
  Pt <- ceiling(Pt/2)
  
  measures <- c(TP,FP,FN,TN,TPt,Pt)
  write.csv(x=measures,row.names = FALSE,
            file = paste(main.dir,sub.dir,'measures.csv',sep='/'))
}
