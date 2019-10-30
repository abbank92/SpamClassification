#apr20

readEmailDirectory <- function(dir) {
  return(lapply(list.files(dir, full.names = TRUE), function(x) readLines(x)))
}
spamEmails <- readEmailDirectory("data/messages/spam")
class(spamEmails)
length(spamEmails)

emailsAllFunc <- function() {
  toReturn <- list()
  for (dir in list.files("data/messages")) {
    mail <- readEmailDirectory(paste0("data/messages/",dir))
    message('check 1 ', dir)
    body <- lapply(mail, function(m) extractBodyText(m))
    message('check 2 ', dir)
    words <- lapply(body, function(b) extractWords(b))
    toReturn <- c(toReturn, words)
  }
  return(toReturn)
}
emailsAll <- emailsAllFunc()
emailsAll[[6031]]

isSpamFunc <- function() {
  toReturn <- rep(FALSE, length(list.files('data/messages/easy_ham')))
  toReturn <- c(toReturn, rep(FALSE, length(list.files('data/messages/easy_ham_2'))))
  toReturn <- c(toReturn, rep(FALSE, length(list.files('data/messages/hard_ham'))))
  toReturn <- c(toReturn, rep(TRUE, length(list.files('data/messages/spam'))))
  return(c(toReturn, rep(TRUE, length(list.files('data/messages/spam_2')))))
}
isSpam <- isSpamFunc()
