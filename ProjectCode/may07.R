#may07

computeBF <- function(words) {
  words <- words[which(words %in% bow)]
  
  bf <- sum(logProbPresentSpam[words]) + 
    sum(logProbAbsentSpam[-which(names(logProbAbsentSpam) %in% words)]) - 
    sum(logProbPresentHam[words]) - 
    sum(logProbAbsentHam[-which(names(logProbPresentHam) %in% words)])
  
  return(bf)
}

msg1Words <- emailsAll[[1]]
computeBF(msg1Words)

bfTest <- sapply(emailsTest, computeBF)
boxplot(bfTest[which(isSpamTest)], bfTest[which(!isSpamTest)], names = c('Spam', 'Not Spam'),
        xlab = 'Test Email Set', ylab = 'log Bayes Factor',
        ylim= c(-500, 500))
abline(h=-30, col='red')




