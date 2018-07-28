
amotre <- function(train.data, perc.over) {
  library(data.table)
  library(dplyr)      
  library(rattle)   
  
  library(caret)
  library(DMwR)
  library(randomForest)
  library(xgboost)
  library(kernlab)
  
  library(amap)

  #3)get majority.train.data and minority.train.data
  tdDim <- dim(train.data)
  fDim <- tdDim[2] - 1
  
  majority.train.data <- train.data[train.data$classLabel!="Anomalous",]
  nDim <- dim(majority.train.data)
  
  minority.train.data <- train.data[train.data$classLabel=="Anomalous",]
  aDim <- dim(minority.train.data)
  
  #4)Apply LOF on each minority instance w.r.t minority class instances excluding this instance
  k.best <- ceiling(sqrt(aDim[1]))
  k.best
  
  #get LOF for each minority instance on k.best
  LOF.i <- lofactor(minority.train.data[,1:16],k=k.best)
  LOF.i
  
  #get sorted LOF.i returns the indices of the instances (default ascending)
  sorted.LOF.i <- order(LOF.i)
  sorted.LOF.i
  
  ceiling.perc.minor.LOF.i <- array(0, dim=aDim[1])
  
  for(a in 1:aDim[1])
  {
    #get the position of the minor instance
    index.minor.LOF.i <- match(a,sorted.LOF.i)
    index.minor.LOF.i
    #calculate percentile of the minor instance w.r.t other minor instances
    #this means the LOF.i for this minor is greater than ceiling.perc.minor.LOF.i % of the LOF.i of other minor instances
    perc.minor.LOF.i <- (index.minor.LOF.i * 100) / aDim[1]
    perc.minor.LOF.i
    ceiling.perc.minor.LOF.i[a] <- ceiling(perc.minor.LOF.i)
    ceiling.perc.minor.LOF.i[a]
  }
  ceiling.perc.minor.LOF.i
  
  #5)Apply LOF on each minority instance w.r.t majority class instances
  
  appendDim <- 1 + nDim[1]
  ceiling.perc.minor.LOF.n <- array(0, dim=aDim[1])
  
  k.best <- ceiling(sqrt(appendDim))
  k.best
  
  #a in 1:aDim[1]
  for(a in 1:aDim[1])
  {  
    minority.train.data[a,]
    minor.append.majorityclass.train.data <- rbind(minority.train.data[a,],majority.train.data)
    minor.append.majorityclass.train.data
    
    #get LOF for each instance on k.best
    LOF.n <- lofactor(minor.append.majorityclass.train.data[,1:16],k=k.best)
    LOF.n
    
    #get sorted LOF.n returns the indices of the instances (default ascending)
    sorted.LOF.n <- order(LOF.n)
    sorted.LOF.n
    
    #get the position of the minor instance (which is first instance)
    index.minor.LOF.n <- match(1,sorted.LOF.n)
    index.minor.LOF.n
    
    #calculate percentile of the minor instance w.r.t major instances
    #this means the LOF.n for this minor is greater than ceiling.perc.minor.LOF.n % of the LOF.n of major instances
    perc.minor.LOF.n <- (index.minor.LOF.n * 100) / appendDim
    perc.minor.LOF.n
    ceiling.perc.minor.LOF.n[a] <- ceiling(perc.minor.LOF.n)
    ceiling.perc.minor.LOF.n[a]
    
    #closeAllConnections()
  }
  ceiling.perc.minor.LOF.n
  
  #6)define a survive.threshold where the minor instance having ceiling.perc.minor.LOF.n 
  #less than survive.threshold is removed from the minor instances in the training set.
  #those instances if not removed would trap the classifier, because they are very close
  #to normal instances
  minor.survive.threshold <- 10 #TUNED parameter
  
  #if survive, keep it 1 (true), if not set it 0 (false)
  boolean.minor.survive <- array(1, dim=aDim[1])
  for(a in 1:aDim[1])
  {
    if(ceiling.perc.minor.LOF.n[a] < minor.survive.threshold)
      boolean.minor.survive[a] <- 0
  }
  boolean.minor.survive
  indices.minor.survive <- which(boolean.minor.survive==1)
  #define the survive_minority.train.data with only the survive minor instances
  minor.survive.aDim <- sum(boolean.minor.survive)
  print(minor.survive.aDim)
  minor.remove.aDim <- aDim[1] - minor.survive.aDim 
  print(minor.remove.aDim)
  
  minor.survive.train.data <- minority.train.data[indices.minor.survive,]
  minor.survive.train.data
  
  #7)Calculate the multiplication of percentiles for only survived instances
  #calculate the probability normalised between [0,1]
  #prob.norm controls p.artificial which is the number of artificial samples to be generated
  
  indices.minor.survive
  ceiling.perc.minor.survive.LOF.i <- ceiling.perc.minor.LOF.i[indices.minor.survive]
  ceiling.perc.minor.survive.LOF.i
  
  ceiling.perc.minor.survive.LOF.n <- ceiling.perc.minor.LOF.n[indices.minor.survive]
  ceiling.perc.minor.survive.LOF.n
  
  prob.norm <- array(0, dim=minor.survive.aDim)
  prob.norm
  
  p.artificial <- array(0, dim=minor.survive.aDim)
  p.artificial
  
  for(sa in 1:minor.survive.aDim)
  {
    mult <- ceiling.perc.minor.survive.LOF.i[sa] * ceiling.perc.minor.survive.LOF.n[sa]
    prob.norm[sa] <- mult /(100*100)
    
  }
  prob.norm
  
  #8)find min.f and max.f for each feature in the minority instances
  max.f <- array(0, dim=fDim)
  min.f <- array(0, dim=fDim)
  max.f <- apply(minor.survive.train.data,2,max)
  min.f <- apply(minor.survive.train.data,2,min)
  max.f
  min.f
  #print(min.f)
  
  
  
  #9)generate new samples
  lambda <- 0.3 #parameter to be tuned
  #prevents generating artificial insider threat instances near the normal instances
  minor.survive.train.data
  majority.train.data
  
  smote.append.minor.Dim <- ((perc.over/100)*aDim[1])+aDim[1]
  print(smote.append.minor.Dim)
  amotre.perc.over <- floor(((smote.append.minor.Dim - minor.survive.aDim)/minor.survive.aDim)*100)
  print(amotre.perc.over)
  art.aDim <- ceiling((amotre.perc.over/100)*minor.survive.aDim)
  print(art.aDim)

  # artificial.data <- matrix(0,art.dDim ,fDim+1)
  artificial.data <- data.frame(matrix(0, nrow=art.aDim , ncol=fDim+1))
  colnames(artificial.data) <- names(train.data)
  
  g <- 0 #index of artificial instance
  while(g < art.aDim)
  {
    genRand <- sample(0:100,1)
    #print(genRand)
    for(sa in 1:minor.survive.aDim)
    {
      if(prob.norm[sa] > (genRand/100))
      {
        #we generate a sample for this minor instance
        g <- g + 1
        if(g > art.aDim)
          break;
        dirRand <- sample(0:100,1)
        #print(dirRand)
        
        for(f in 1:fDim)################################################
        {
          nearest.pos.distance <- 0
          nearest.neg.distance <- 0
          distances <- array(0,nDim[1])
          
          for(n in 1:nDim[1])
          {
            distances[n] <- as.numeric(minor.survive.train.data[[sa,f]] - majority.train.data[[n,f]])
            #print(distances[n])
          }
          pos.distances <- distances[distances >= 0]
          #print(pos.distances)
          neg.distances <- distances[distances < 0]
          #print(neg.distances)
          
          if(is.array(pos.distances)==TRUE & is.array(neg.distances)==TRUE){ 
            #generate artificial instances in both pos & neg directions 
            prob.pos <- 0.5
            prob.neg <- 0.5
            
            nearest.pos.distance <- min(pos.distances)
            #print(nearest.pos.distance)
            nearest.neg.distance <- max(neg.distances)
            #print(nearest.neg.distance)
            
            if(dirRand < (prob.pos/100)){
              op <- +1
              lambda <- 0.3
              artificial.data[[g,f]] <- minor.survive.train.data[[sa,f]] + op * runif(1,0.0,lambda * nearest.pos.distance)
             
            }else{
              op <- -1
              lambda <- 0.3
              artificial.data[[g,f]] <- minor.survive.train.data[[sa,f]] + op * runif(1,0.0,lambda * abs(nearest.neg.distance))
              }
    
            
          } else if(is.array(pos.distances)==TRUE){ 
            prob.pos <- 0.2
            prob.neg <- 0.8
            nearest.pos.distance <- min(pos.distances)
            #print(nearest.pos.distance)
            
            if(dirRand < (prob.pos/100)){
              op <- +1
              lambda <- 0.3
            }else{
              op <- -1
              lambda <- 1
            }
            artificial.data[[g,f]] <- minor.survive.train.data[[sa,f]] + op * runif(1,0.0,lambda * nearest.pos.distance)
           
          } else{ 
            prob.pos <- 0.8
            prob.neg <- 0.2
            nearest.neg.distance <- max(neg.distances)
            #print(nearest.neg.distance)
            
            if(dirRand < (prob.neg/100)){
              op <- -1
              lambda <- 0.3
            }else{
              op <- +1
              lambda <- 1
            }
            #print(minor.survive.train.data[[sa,f]])
            artificial.data[[g,f]] <- minor.survive.train.data[[sa,f]] + op * runif(1,0.0,lambda * abs(nearest.neg.distance))
            #print(artificial.data[[g,f]])
            }

            if(artificial.data[[g,f]] < 0) #to avoid negative features
               artificial.data[[g,f]] <- as.numeric(min.f[f])
        }
     
        #add label 'Anomalous' to artificial data
        artificial.data[[g,fDim+1]] <- 'Anomalous'
 
      }
    }
  }

  #bind minor.survive.train.data_label with artificial.data generated and artificial.data
  return.data <- rbind(minor.survive.train.data, artificial.data)
  return.data$classLabel <- as.factor(return.data$classLabel)
  return.data
  
  return(return.data)
}


