#may04

set.seed(0)
getTrainInds <- function(emailsAll) {
  numTrain <- ceiling(length(emailsAll) * 2 / 3)
  numHam <- floor(.75 * numTrain)
  numSpam <- numTrain-numHam
  nums <- sapply(list.files("data/messages"), function(x) length(list.files(paste0("data/messages/", x))))
  totalHam <- sum(nums[1:3])
  totalSpam <- sum(nums[4:5])
  s1 <- sample(1:totalHam, numHam)
  s2 <- sample(1:totalSpam, numSpam) + totalHam
  return(c(s1, s2))
}
inds <- getTrainInds(emailsAll)
emailsTrain <- emailsAll[inds]
isSpamTrain <- isSpam[inds]
emailsTest <- emailsAll[-inds]
isSpamTest <- isSpam[-inds]

length(emailsTrain) / length(emailsAll)
c(mean(isSpam), mean(isSpamTrain), mean(isSpamTest))


bowFunc <- function(emailsTrain) {
  bow <- unique(unlist(emailsTrain))
}
bow <- bowFunc(emailsTrain)


tSpam <- table(unlist(emailsTrain[which(isSpamTrain)]))
tHam <- table(unlist(emailsTrain[which(!isSpamTrain)]))

bowSpam <- numeric(length(bow)); names(bowSpam) <- bow
bowHam <- numeric(length(bow)); names(bowHam) <- bow

bowSpam[names(tSpam)] <- tSpam
bowHam[names(tHam)] <- tHam

s1 <- sum(isSpamTrain)
probPresentSpam <- sapply(bowSpam, function(w) (w+.01)/(s1+.01))
probAbsentSpam <- 1-probPresentSpam

s2 <- sum(!isSpamTrain)
probPresentHam <- sapply(bowHam, function(w) (w+.01)/(s2+.01))
probAbsentHam <- 1-probPresentHam

logProbPresentSpam <- log(probPresentSpam)
logProbPresentHam  <- log(probPresentHam)
logProbAbsentSpam  <- log(probAbsentSpam)
logProbAbsentHam   <- log(probAbsentHam)

#Deliverable 6
#a
probPresentHam['monday']/probPresentSpam['monday']
#b
probPresentSpam['buy']/probPresentHam['buy']



