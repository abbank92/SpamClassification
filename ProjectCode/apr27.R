#apr27

paste(test, collapse = ' ')

extractWords <- function(body) {
  #Uses deparse to take care of non-character encodings
  txt <- paste(body, collapse = ' ')
  txt <- deparse(txt)
  txt <- gsub('[[:punct:][:digit:][:blank:]]+', ' ', txt)
  txt <- tolower(txt)
  txt <- unlist(strsplit(txt, ' '))
  txt <- unique(txt)
  txt <- txt[which(nchar(txt)>1)]
  return(txt)
}

extractWords(test)
