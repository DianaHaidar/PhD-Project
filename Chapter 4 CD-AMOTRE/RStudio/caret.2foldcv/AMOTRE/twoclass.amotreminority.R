twoclass.amotreminority <- function(data,method,perc.over) {
  
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
  
  library(amap)
  
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
    
    #amotre
    #save majority.train (not to amotre)
    majority.train.data <- train.data[train.data$classLabel=="Normal",]
    #apply amotre on train.data
    set.seed(42)
    source("amotre.R")
    amotre.train.data <- amotre(train.data[,1:17],perc.over)
    #extract amotred minority.train.data (original+artificial)
    amotre.minority.train.data <- amotre.train.data[amotre.train.data$classLabel=="Anomalous",]
    #append amotre.minority.train + majority.train
    amotre.minority.train.append.majority.train.data <- rbind(amotre.minority.train.data, majority.train.data,fill=TRUE)
    train.data <- amotre.minority.train.append.majority.train.data
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
    FP <- FP + cm.table[3]
    FN <- FN + cm.table[2]
    TN <- TN + cm.table[4]
    
    
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
