#apr25

extractBodyText <- function(email) {
  msg <- splitMessage(email)
  if (hasAttachment(msg$header)) {
    boundary <- getBoundary(msg$header)
    inds <- grep(boundary, msg$body, fixed = TRUE)
    if (length(inds) == 1) {
      return(msg$body[(inds+1):length(msg$body)])
    }
    if(is.na(inds[1] +1) | is.na(inds[2]-1)) {print(inds); print(boundary)}
    #otherwise assume we get three hits
    return(msg$body[(inds[1]+1):(inds[2]-1)])
  }
  return(msg$body)
}


#Testing
test <- extractBodyText(msg3)
msg15 <- readLines(files[15])
test <- extractBodyText(msg15)
#Dope!

#Deliverable 3 ¡¡¡¡¡UNFINISHED!!!!
files <- list.files('data/messages')
getWordDist <- function(x) { 
  emails <- list.files(paste0('data/messages/',x), full.names = TRUE)
  dist <- sapply(emails, function(e) getLogWordCount(e))
  return(dist)
}
getLogWordCount <- function(e) {
  txt <- extractBodyText(e)
  txt <- gsub('[ \t\n\r\v\f]+', ' ', txt)
  txt
  print(txt)
  txt <- strsplit(txt, ' ')
  return(log(length(txt)))
}


dist <- sapply(emailers, function(e) {
  txt <- extractBodyText(e)
  
})