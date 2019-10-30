#apr23

hasAttachment <- function(header) {
  ct <- grep("Content-Type:", header, value = TRUE)
  if (length(ct)==0) return(FALSE)
  return (grepl("multipart", tolower(ct)))
  return(FALSE)
}

#testing
sampleHeaders <- sapply(sampleEmails, function(x) splitMessage(x)$header)
hasHeader <- sapply(sampleHeaders, function(x) hasAttachment(x))
emailIndWithAttachments <- which(hasHeader)
emailIndWithAttachments

getBoundary <- function(header) {
  bline <- grep("boundary=", header, value = TRUE)
  if(length(bline) < 1) {
    bline <- grep("BOUNDARY=", header, value = TRUE)
    boundary <- strsplit(bline, "OUNDARY=")
  }
  else {boundary <- strsplit(bline, "oundary=")}
  #if(length(boundary) < 1) boundary <- strsplit(bline, "OUNDARY=")
  #if(length(boundary) < 1) {print(boundary); print(header)}
  boundary <- boundary[[1]][2]
  boundary <- gsub('^[ ";]+', '', boundary)
  boundary <- gsub('[ ";]+$', '', boundary)
  if(grepl('";.*$', boundary)) boundary <- gsub('";.*$', '', boundary)
  return(boundary)
}

#testing2
headersWithAttachments <- sampleHeaders[emailIndWithAttachments]
boundaries <- paste0(sapply(headersWithAttachments,
                            function(x) paste0(getBoundary(x), "\n")),
                     collapse = "")
cat(boundaries)
identical('_NextPart_1_bvfoDiTVghtoCXFdvJNKcuWblFV', '_NextPart_1_bvfoDiTVghtoCXFdvJNKcuWblFV')
