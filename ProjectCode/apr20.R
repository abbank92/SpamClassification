#apr20
#lmao

setwd("/Users/alexbank/Documents/stat359/SpamClassification")
list.files("data/messages")
files <- list.files("data/messages/easy_ham")
length(files)

files <- list.files("data/messages/easy_ham", full.names = TRUE)
head(files)

msg3 <- readLines(files[3])

#First 500 emails
files <- list.files("data/messages/easy_ham", full.names = TRUE)
sampleIndex <- 1:500
sampleFiles <- files[sampleIndex]
sampleEmails <- lapply(sampleFiles, readLines)

#Split Message
splitMessage <- function(msg) {
  split <- which(msg == "")[1]
  #if(is.na(split)) print(msg)
  header <- msg[1:(split-1)]
  body <- msg[(split+1):length(msg)]
  return(list(header=header, body=body))
}
splitmsg3 <- splitMessage(msg3)
splitmsg3

#deliverable 1
sapply(list.files("data/messages"), function(x) length(list.files(paste0("data/messages/", x))))
