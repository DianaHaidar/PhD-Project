amotreminority.decomposeall <- function(data,method,perc.over) {
  
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
    #save majority.train (to decompose/not to amotre)
    majority.train.data <- train.data[train.data$classLabel=="Normal",]
    #apply amotre on train.data
    set.seed(42)
    source("amotre.R")
    amotre.train.data <- amotre(train.data[,1:17],perc.over=perc.over)
    #extract amotred minority.train.data (original+artificial)
    amotre.minority.train.data <- amotre.train.data[amotre.train.data$classLabel=="Anomalous",]
    ###
    
    ###PREPROCESS APPEND
    #save minority.test
    minority.test.data <- test.data[test.data$classLabel=="Anomalous",]
    #save majority.test
    majority.test.data <- test.data[test.data$classLabel=="Normal",]
    #append amotre.minority.train.append.minority.test
    amotre.minority.train.append.minority.test.data <- rbind(amotre.minority.train.data,minority.test.data,fill=TRUE)
    minority.data <- amotre.minority.train.append.minority.test.data 
    #append majority.train.append.majority.test.data
    majority.train.append.majority.test.data <- rbind(majority.train.data,majority.test.data,fill=TRUE)
    majority.data <- majority.train.append.majority.test.data
    #get size of amotre minority train data
    dim.amotre.minority.train.data <- dim(amotre.minority.train.data)
    dim.amotre.minority.train.data[1]
    #get size of majority train data
    dim.majority.train.data <- dim(majority.train.data)
    dim.majority.train.data[1]
    ###
    
    ###DECOMPOSE MAJORITY
    #decompose
    set.seed(20)
    majority.clusters <- kmeans(majority.data[,1:16], 2, nstart = 20)
    majority.clusters$cluster <- as.factor(majority.clusters$cluster)
    clusterLabel <- sub("^", "nCluster", majority.clusters$cluster )
    majority.data$classLabel <- clusterLabel
    majority.data$classLabel <- as.factor(majority.data$classLabel)
    ###
    
    ###DECOMPOSE MINORITY
    #decompose
    set.seed(20)
    minority.clusters <- kmeans(minority.data[,1:16], 2, nstart = 20)
    minority.clusters$cluster <- as.factor(minority.clusters$cluster)
    clusterLabel <- sub("^", "aCluster", minority.clusters$cluster )
    minority.data$classLabel <- clusterLabel
    minority.data$classLabel <- as.factor(minority.data$classLabel)
    ###
    
    ###POSTPROCESS SPLIT
    #split into train and test data based on saved size
    dim.minority.data <- dim(minority.data)
    amotre.minority.train.data <- minority.data[1:dim.amotre.minority.train.data[1],]
    minority.test.data <- minority.data[dim.amotre.minority.train.data[1]:dim.minority.data[1],]
    dim.minority.test.data <- dim(minority.test.data)

    dim.majority.data <- dim(majority.data)
    majority.train.data <- majority.data[1:dim.majority.train.data[1],]
    majority.test.data <- majority.data[dim.majority.train.data[1]:dim.majority.data[1],]
    dim.majority.test.data <- dim(majority.test.data)
    
    train.data <- rbind(amotre.minority.train.data,majority.train.data)
    test.data <- rbind(minority.test.data[2:dim.minority.test.data[1]],majority.test.data[2:dim.majority.test.data[1]])
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
    TP <- TP + cm.table[1] + cm.table[2] + cm.table[5] + cm.table[6]
    FP <- FP + cm.table[9] + cm.table[10] + cm.table[13] + cm.table[14]
    FN <- FN + cm.table[3] + cm.table[4] + cm.table[7] + cm.table[8]
    TN <- TN + cm.table[11] + cm.table[12] + cm.table[15] + cm.table[16]
    
    
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
        if(m.predict[t.index]=='aCluster1' || m.predict[t.index]=='aCluster2')
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
