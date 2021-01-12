

# ************************* helping functions *************************************

get.sentences.with.integer <- function(aText) {
  sentences <- unlist(text2sentences(aText))
  intPattern <- "( |^)[0-9]+,?[0-9]+( |,|;|\\)|\\.$)"
  return(sentences[grep(intPattern, sentences)])
}

remove.na <- function (aVector){
  vectorWithoutNA <- aVector[!is.na(aVector)]
  return(vectorWithoutNA)
}

contains.element <- function(chr_vector, element){
  #returns boolean (TRUE if element is contained in chr_vector FALSE otherwise)
  newVec <- chr_vector[chr_vector == element]
  return(length(newVec > 0))
}

is.measurementUnit <- function(aToken) {
  keys <- c("db", "sec", "ms", "min", "m", "mm", "cm", "km", "km/h", "g", "kg", "seconds", "milliseconds","minutes", "hr", "hrs", "hz")
  return(contains.element(keys, tolower(aToken)))
}

is.month <- function(aToken){
  keys <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  return(contains.element(keys, tolower(aToken)))
}


currentNgram <- function (anIndex, aVector, lowerLimit, upperLimit){
  currNgram <- c()
  for(i in lowerLimit:upperLimit){
    if(anIndex + i > 0)
      currNgram <- append(currNgram,aVector[anIndex + i])
  }
  currNgram <- remove.na(currNgram)
  return(paste(currNgram, collapse = " "))
}
