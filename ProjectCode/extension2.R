#real shit extension

emailsTrain
isSpamTrain

#single cross validation

#first we split the training set into two
cvSplitInds <- function(){
  spamInds <- which(isSpamTrain)
  nonSpamInds <- which(!isSpamTrain)
  
  inds1 <- sample(spamInds, floor(length(spamInds)/2))
  inds2 <- sample(nonSpamInds, floor(length(nonSpamInds)/2))
  
  return(c(inds1, inds2))
}

cvInds <- cvSplitInds()

emailsCV <- emailsTrain[cvInds]
isSpamCV <- isSpamTrain[cvInds]

emailsTrainCV <- emailsTrain[-cvInds]
isSpamTrainCV <- isSpamTrain[-cvInds]

#now we train the data using emailsTrainCV
bow <- bowFunc(emailsTrainCV)

tSpam <- table(unlist(emailsTrainCV[which(isSpamTrainCV)]))
tHam <- table(unlist(emailsTrainCV[which(!isSpamTrainCV)]))

bowSpam <- numeric(length(bow)); names(bowSpam) <- bow
bowHam <- numeric(length(bow)); names(bowHam) <- bow

bowSpam[names(tSpam)] <- tSpam
bowHam[names(tHam)] <- tHam

s1 <- sum(isSpamTrainCV)
probPresentSpam <- sapply(bowSpam, function(w) (w+.01)/(s1+.01))
probAbsentSpam <- 1-probPresentSpam

s2 <- sum(!isSpamTrainCV)
probPresentHam <- sapply(bowHam, function(w) (w+.01)/(s2+.01))
probAbsentHam <- 1-probPresentHam

logProbPresentSpam <- log(probPresentSpam)
logProbPresentHam  <- log(probPresentHam)
logProbAbsentSpam  <- log(probAbsentSpam)
logProbAbsentHam   <- log(probAbsentHam)

#now we calculate c using emailsCV and isSpamCV
bfCV <- sapply(emailsCV, computeBF)

c <- -500:500
t1error <- numeric(length(c))
t2error <- numeric(length(c))
for (i in 1:length(c)) {
  t1error[i] <- length(which(bfCV[which(!isSpamCV)] > c[i])) / sum(!isSpamCV)
  t2error[i] <- length(which(bfCV[which(isSpamCV)] <= c[i])) / sum(isSpamCV)
}
bestCInd <- which.min(abs(t1error-t2error))
bestC <- c[bestCInd]

#now we evaluate how well this worked on our test set
t1errorTest <- length(which(bfTest[which(!isSpamTest)] > bestC)) / sum(!isSpamTest)
t2errorTest <- length(which(bfTest[which(isSpamTest)] <= bestC)) / sum(isSpamTest)


