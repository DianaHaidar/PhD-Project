ensemble.ocsvm.update.FP.chunk.oversample <- function(data,e.size,kmeans.method,kernel,FP.chunk.size,perc.over) {
  
  main.dir <- 'C:/Users/research/Documents/RStudio/workspace/oneclass.test'
  sub.dir <- paste(e.size,kernel,FP.chunk.size,perc.over,sep=',')
  dir.create(file.path(main.dir, sub.dir))
  
  library(data.table)
  library(dplyr)   
  library(caret)
  library(e1071)
  library(Rlof)
  library(amap)
  library(DMwR2)
  
majority.data <- data[data$classLabel=='Normal',]
summary(majority.data$classLabel)
minority.data <- data[data$classLabel=='Anomalous',]
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
  # write.csv(x=test.data.threat.label,row.names = FALSE,
  #           file = paste(main.dir,sub.dir,test.fold.file,sep='/'))
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
  FP.chunk <- rep(list(NULL),e.size)
  cluster.data.append.syn.data <- rep(list(NULL),e.size)
  
  for(inst in 1:dimT)
  {
    
    test.instance <- test.data[inst,]
    #calculate svm.pred.bool for each test instance
    #save for vote
    library(caret)
    
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
        FP.chunk[[e]] <- rbind(FP.chunk[[e]],test.instance)
        #if(FP%%FP.chunk.size ==0)
        if((FP!=0) && (FP%%FP.chunk.size ==0))
        {
          #step1: calculate LOF.n for each FP.instance in FP.chunk 
          #w.r.t real negative instances (train.data + previous FP.chunk(s) + current FP.chunk) 
          #not including synthetic samples
          
          cluster.dim <-  dim(cluster[[e]])
          append.dim <- 1 + cluster.dim[1]
          f.dim <- cluster.dim[2] - 1
          ceil.FP.perc.LOF.n <- array(0, dim=FP.chunk.size)
          
          k.best <- ceiling(sqrt(append.dim))
          k.best
          
          #a in 1:aDim[1]
          for(FP.index in 1:FP.chunk.size)
          { 
            
            FP.chunk[[e]][FP.index,]
            FP.instance.append.cluster.data <- rbind(FP.chunk[[e]][FP.index,],cluster[[e]])
            FP.instance.append.cluster.data
            
            #get LOF.n for each instance on k.best
            #LOF.n <- lof(FP.instance.append.train.data[,1:16],k=k.best,cores=1)
            LOF.n <- lofactor(FP.instance.append.cluster.data[,1:16],k=k.best)
            LOF.n
            
            #get sorted LOF.n returns the indices of the instances (default ascending)
            sorted.LOF.n <- order(LOF.n)
            sorted.LOF.n
            
            #get the position of the FP instance
            index.FP.LOF.n <- match(FP.index,sorted.LOF.n)
            index.FP.LOF.n
            
            #calculate percentile of the FP instance w.r.t other negative instances
            #this means the LOF.n for this FP > ceil.FP.perc.LOF.n % of the LOF.n of other negative instances
            FP.perc.LOF.n <- (index.FP.LOF.n * 100) / append.dim
            FP.perc.LOF.n
            ceil.FP.perc.LOF.n[FP.index] <- ceiling(FP.perc.LOF.n)
            ceil.FP.perc.LOF.n[FP.index]
            
            #closeAllConnections()
          }#end for(FP.index in 1:FP.chunk.size)
          ceil.FP.perc.LOF.n
          
          #step2: normalise LOF for each FP.instance in FP.chunk
          prob.norm <- array(0, dim=FP.chunk.size)
          prob.norm
          
          p.syn <- array(0, dim=FP.chunk.size)
          p.syn
          
          for(FP.index in 1:FP.chunk.size)
          { 
            prob.norm[FP.index] <- ceil.FP.perc.LOF.n[FP.index]/100
            
          }
          prob.norm
          
          #find min.f for each feature in the FP instances
          min.f <- array(0, dim=f.dim)
          min.f <- apply(FP.chunk[[e]],2,min)
          min.f
          #print(min_f)
          
          #step3: calculate probability of sampling FP.instance proportional to LOF
          #the lower LOF, the higher its chance to be oversampled
          lambda <- 0.8 #parameter can be tuned
          
          #step4: oversample at feature level, s.t. at the level of each feature,
          #you get closer to closest instance by maximum 0.8 distance why??
          
          syn.dim <- ceiling((perc.over/100)*FP.chunk.size)
          #print(syn.dim)
          
          syn.data <- data.frame(matrix(0, nrow=syn.dim , ncol=f.dim+1))
          colnames(syn.data) <- names(cluster[[e]])
          
          g <- 0 #index of synthetic instance
          while(g < syn.dim)
          {
            gen.rand <- sample(0:100,1)
            #print(gen.rand)
            
            for(FP.index in 1:FP.chunk.size)
            { 
              if(prob.norm[FP.index] < (gen.rand/100))
              {
                #we generate a sample for this FP instance
                g <- g + 1
                if(g > syn.dim)
                  break;
                dir.rand <- sample(0:100,1)
                #print(dir.rand)
                
                for(f in 1:f.dim)
                {
                  nearest.pos.dist <- 0
                  nearest.neg.dist <- 0
                  distances <- array(0,cluster.dim[1])
                  
                  for(n in 1:cluster.dim[1])
                  {
                    distances[n] <- as.numeric(FP.chunk[[e]][[FP.index,f]] - cluster[[e]][[n,f]])
                    #print(distances[n])
                  }
                  
                  pos.distances <- distances[distances >= 0]
                  #print(pos.distances)
                  neg.distances <- distances[distances < 0]
                  #print(neg.distances)
                  
                  if(is.array(pos.distances)==TRUE & is.array(neg.distances)==TRUE){ 
                    #generate synthetic instances in both pos & neg directions 
                    prob.pos <- 0.5
                    prob.neg <- 0.5
                    
                    nearest.pos.dist <- min(pos.distances)
                    #print(nearest.pos.dist)
                    nearest.neg.dist <- max(neg.distances)
                    #print(nearest.neg.dist)
                    
                    if(dir.rand < (prob.pos/100)){
                      op <- +1
                      lambda <- 0.8
                      syn.data[[g,f]] <- FP.chunk[[e]][[FP.index,f]] + op * runif(1,0.0,lambda * nearest.pos.dist)
                      
                    }else{
                      op <- -1
                      lambda <- 0.8
                      syn.data[[g,f]] <- FP.chunk[[e]][[FP.index,f]] + op * runif(1,0.0,lambda * abs(nearest.neg.dist))
                    }
                  } else if(is.array(pos.distances)==TRUE){ 
                    nearest.pos.dist <- min(pos.distances)
                    #print(nearest.pos.dist)
                    op <- +1
                    syn.data[[g,f]] <- FP.chunk[[e]][[FP.index,f]] + op * runif(1,0.0,lambda * nearest.pos.dist)
                    
                  } else{ 
                    nearest.neg.dist <- max(neg.distances)
                    #print(nearest.neg.dist)
                    op <- -1
                    #print(minor_survive_train_data[[sa,f]])
                    syn.data[[g,f]] <- FP.chunk[[e]][[FP.index,f]] + op * runif(1,0.0,lambda * abs(nearest.neg.dist))
                    #print(syn.data[[g,f]])
                  }
                  
                  if(syn.data[[g,f]] < 0) #to avoid negative features
                    syn.data[[g,f]] <- as.numeric(min.f[f])
                }
                
                #add label 'Anomalous' to artificial data
                syn.data[[g,f.dim+1]] <- 'Anomalous'
              }#endif
            }#endfor
          }#endwhile
          
          #step5: append train.data to FP.chunk
          cluster[[e]] <- rbind(cluster[[e]], FP.chunk[[e]])
          
          #step6: append train.data (already appended to FP.chunk) + generated synthetic instances
          cluster.data.append.syn.data[[e]] <- rbind(cluster[[e]], syn.data)
          
          #step7: update (retrain) ocsvm model with train.data.append.syn.data
          set.seed(42)
          svm.model[[e]]<-svm(cluster.data.append.syn.data[[e]][,1:16],y=NULL,
                         type='one-classification',
                         nu=0.5,
                         scale=TRUE,
                         kernel=kernel)
          
        }#end if((FP!=0) 
      }#end if(test.instance.label!='Anomalous'
    }# end for(inst in 1:dimT)
    
  ######################################################
  
  predict.fold.file <- paste('fold',f.test.num,'.predict.data.class.label.csv',sep='')
  # write.csv(x=svm.pred,row.names = FALSE,
  #           file = paste(main.dir,sub.dir,predict.fold.file,sep='/'))
  
  
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
  
}# end for(f.test in folds.data)
TP.avg <- ceiling(TP.avg/10)
FP.avg <- ceiling(FP.avg/10)
FN.avg <- ceiling(FN.avg/10)
TN.avg <- ceiling(TN.avg/10)

TPt.avg <- ceiling(TPt.avg/10)

measures <- c(TP.avg,FP.avg,FN.avg,TN.avg,TPt.avg)
write.csv(x=measures,row.names = FALSE,
          file = paste(main.dir,sub.dir,'measures.csv',sep='/'))
}

