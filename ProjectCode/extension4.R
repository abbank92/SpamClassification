#extension4

testmsg <- emailsAll[[400]]
stemDocument(testmsg)

'%!in%' <- function(x,y)!('%in%'(x,y))

newExtractWords <- function(body) {
  txt <- paste(body, collapse = ' ')
  txt <- deparse(txt)
  #get rid of blank spaces
  txt <- gsub('[[:blank:]]+', ' ', txt)
  
  #get rid of email addresses
  txt <- gsub(' [[:alnum:]._-]+@[[:alnum:].-]+ ', ' ', txt)
  
  #get rid of URLs
  txt <- gsub('(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,}\\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)',
              ' ', txt)
  
  #get rid of numbers, blank spaces, punctuation
  txt <- gsub('[[:digit:][:blank:][:punct:]]+', ' ', txt)
  
  txt <- tolower(txt)
  txt <- unlist(strsplit(txt, ' '))
  txt <- unique(txt)
  txt <- txt[which(nchar(txt)>1)]
  
  #use tm package to stem the words
  txt <- stemDocument(txt)
  
  #get rid of stop words
  txt <- txt[which(txt %!in% stopwords())]
  
  return(txt)
}

emailsAllFunc2 <- function() {
  toReturn <- list()
  for (dir in list.files("data/messages")) {
    mail <- readEmailDirectory(paste0("data/messages/",dir))
    message('check 1 ', dir)
    body <- lapply(mail, function(m) extractBodyText(m))
    message('check 2 ', dir)
    words <- lapply(body, function(b) newExtractWords(b))
    toReturn <- c(toReturn, words)
  }
  return(toReturn)
}


emailsAll2 <- emailsAllFunc2()
testmsg2 <- emailsAll2[[400]]
testmsg
testmsg2
