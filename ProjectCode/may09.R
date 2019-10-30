#may09

plotErrorRates <- function() {
  c <- -500:500
  t1error <- numeric(length(c))
  t2error <- numeric(length(c))
  for (i in 1:length(c)) {
    t1error[i] <- length(which(bfTest[which(!isSpamTest)] > c[i])) / sum(!isSpamTest)
    t2error[i] <- length(which(bfTest[which(isSpamTest)] <= c[i])) / sum(isSpamTest)
  }
  plot(x=c, y=t1error, type='l', lwd=2, col='red', ylab='Error Rates', xlab='Cutoff Threshold (c)',
       main="Error Rates by Cutoff Threshold")
  lines(x=c, y=t2error, col='blue', lwd=2)
  abline(v=-50, lty=2)
  legend('topright', col=c('red','blue'), lty=1, lwd = 2,
         legend=c('Type I Error Rate (false alarms)','Type II Error Rate (missed detections)'),
         cex=.6)
}

plotErrorRates()
