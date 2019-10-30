#other code

#deliverable 3

extractAllWords <- function(body) {
  #Uses deparse to take care of non-character encodings
  txt <- paste(body, collapse = ' ')
  txt <- deparse(txt)
  txt <- gsub('[[:punct:][:digit:][:blank:]]+', ' ', txt)
  txt <- tolower(txt)
  txt <- unlist(strsplit(txt, ' '))
  return(txt)
}

numWordsDir <- function(dir) {
  emails <- list.files(paste0('data/messages/',dir), full.names = TRUE)
  emails <- sapply(emails, readLines)
  emails <- sapply(emails, extractBodyText)
  emails <- sapply(emails, extractAllWords)
  emails <- sapply(emails, length)
  return(emails)
}


ehWC <- log(numWordsDir('easy_ham'))
eh2WC <- log(numWordsDir('easy_ham_2'))
hhWC <- log(numWordsDir('hard_ham'))
sWC <- log(numWordsDir('spam'))
s2WC <- log(numWordsDir('spam_2'))

boxplot(ehWC, eh2WC, hhWC, sWC, s2WC, names = c('Easy Ham', 'Easy Ham 2', 'Hard Ham', 'Spam', 'Spam 2'),
        xlab='Directory', ylab='Log of Word Count', main='Word Count by Directory')

#Deliverable 4
numUnique <- lapply(emailsAll, length)
p1 <- hist(unlist(numUnique)[which(isSpam)], freq = FALSE, breaks = seq(0, 10^6, by=50))
p2 <- hist(unlist(numUnique)[which(!isSpam)], freq = FALSE, breaks = seq(0,10^6,by=50))
plot(p2, col=rgb(0,0,1,1/4), xlim = c(0, 1000), freq = FALSE, xlab = 'Word Count', main = 'Histogram of Word Count')
plot(p1, col = rgb(1,0,0,1/4), xlim=c(0,1000), freq=FALSE,add = TRUE)
legend('topright',fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)), legend = c('Non-Spam','Spam'))

#Deliverable 9
#a
c <- -500:500
t1error <- numeric(length(c))
t2error <- numeric(length(c))
for (i in 1:length(c)) {
  t1error[i] <- length(which(bfTest[which(!isSpamTest)] > c[i])) / sum(!isSpamTest)
  t2error[i] <- length(which(bfTest[which(isSpamTest)] <= c[i])) / sum(isSpamTest)
}
bestCInd <- which.min(abs(t1error-t2error))
bestC <- c[bestCInd]
#b
message('Type I Error Rate: ', t1error[bestCInd],
        '\nType II Error Rate: ', t2error[bestCInd])

#Deliverable 10
smallIndexes <- which(t1error<.001)
bestIndex <- which(t2error == min(t2error[smallIndexes]))[2]
restrictedC <- c[bestIndex]
t1error[bestIndex]
t2error[bestIndex]
