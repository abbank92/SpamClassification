#extension3

#k fold cross validation

#first we split the training set into k subsets by indicies
kFoldSplits <- function(k) {
  spamIndexes    <- which(isSpamTrain)
  nonSpamIndexes <- which(!isSpamTrain)
  
  numSpam <- length(spamIndexes)
  numHam  <- length(nonSpamIndexes)
  
  toReturn <- list()
  
  for (i in 1:k) {
    inds1 <- sample(1:length(spamIndexes), floor(numSpam/k))
    inds2 <- sample(1:length(nonSpamIndexes), floor(numHam/k))
    
    toReturn[[i]] <- c(spamIndexes[inds1], nonSpamIndexes[inds2])
    
    spamIndexes <- spamIndexes[-inds1]
    nonSpamIndexes <- nonSpamIndexes[-inds2]
  }
  
  return(toReturn)
}

#kFoldInds <- kFoldSplits(3)

findC <- function(k){
  eureka <- c()
  c <- -500:500
  t1error <- numeric(length(c))
  t2error <- numeric(length(c))
  
  #first we split
  kInds <- kFoldSplits(k)
  
  for (i in 1:k) {
    message('Check1: ',i)
    #we split into training and CV
    trainingInds <- unlist(kInds[-i])
    
    tempTrain <- emailsTrain[trainingInds]
    isSpamTempTrain <- isSpamTrain[trainingInds]
    
    tempCV <- emailsTrain[-trainingInds]
    isSpamTempCV <- isSpamTrain[-trainingInds]
    
    #we train using tempTrain and isSpamTempTrain
    bow <- bowFunc(tempTrain)
    
    tSpam <- table(unlist(tempTrain[which(isSpamTempTrain)]))
    tHam <- table(unlist(tempTrain[which(!isSpamTempTrain)]))
    
    bowSpam <- numeric(length(bow)); names(bowSpam) <- bow
    bowHam <- numeric(length(bow)); names(bowHam) <- bow
    
    bowSpam[names(tSpam)] <- tSpam
    bowHam[names(tHam)] <- tHam
    
    s1 <- sum(isSpamTempTrain)
    probPresentSpam <- sapply(bowSpam, function(w) (w+.01)/(s1+.01))
    probAbsentSpam <- 1-probPresentSpam
    
    s2 <- sum(!isSpamTempTrain)
    probPresentHam <- sapply(bowHam, function(w) (w+.01)/(s2+.01))
    probAbsentHam <- 1-probPresentHam
    
    logProbPresentSpam <- log(probPresentSpam)
    logProbPresentHam  <- log(probPresentHam)
    logProbAbsentSpam  <- log(probAbsentSpam)
    logProbAbsentHam   <- log(probAbsentHam)
    
    message('Check2 done training: ',i)
    
    #now we calculate c using tempCV and isSpamTempCV
    bfCV <- sapply(tempCV, computeBF)

    for (i in 1:length(c)) {
      t1error[i] <- length(which(bfCV[which(!isSpamTempCV)] > c[i])) / sum(!isSpamTempCV)
      t2error[i] <- length(which(bfCV[which(isSpamTempCV)] <= c[i])) / sum(isSpamTempCV)
    }
    bestCInd <- which.min(abs(t1error-t2error))
    buenoC <- c[bestCInd]
    eureka <- c(eureka, buenoC)
    message('Check3 got a c: ',i)
    print(eureka)
  }
  
  return(mean(eureka))
}


#now we see how it did
mejorC <- findC(3)
t1errorTest <- length(which(bfTest[which(!isSpamTest)] > mejorC)) / sum(!isSpamTest)
t2errorTest <- length(which(bfTest[which(isSpamTest)] <= mejorC)) / sum(isSpamTest)








