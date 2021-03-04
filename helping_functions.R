
# ************************* helping functions *************************************

get.sentences.with.integer <- function(aText) {
  sentences <- unlist(text2sentences(aText))
  intPattern <- "( |\\(|^)[0-9]+(,[0-9]+)?( |,|;|\\)|\\.$)"
  return(sentences[grep(intPattern, sentences)])
}


get.measurePattern <- function(){
  #returns measurement units collapsed by | for regex-pattern search
  return (" ?(\\°|db|(milli)?sec(onds)?|(n|m)?s|min(utes)?|m?m|cm|km(/h)?|g|k?g(/m ?2)?|h(r|rs|z)?)( |\\)|,|;)")
}

get.monthPattern <- function(){
  #returns all months collapsed by | for regex-pattern search
  return ("(january|february|march|april|may|june|july|august|september|october|november|december)")
}

get.exclusionKeyPattern <- function(){
  #indicators that number is not samplesize
  return ("[0-9]+ (study|analyses|factors?|different|experiments?|waves?|times?|tasks?|steps?|hours?|days?|weeks?|months?|years?|sessions?|items?|factors?|groups?|trials?|scales?|points?|records|databases|articles|categories|concepts|schools|communities|classrooms|classes|basic)( |\\)|\\.|,|;)")
}

currentNgram <- function (anIndex, aVector, lowerLimit, upperLimit){
  currNgram <- c()
  for(i in lowerLimit:upperLimit){
    if(anIndex + i > 0)
      currNgram <- append(currNgram,aVector[anIndex + i])
  }
  currNgram <- currNgram[!is.na(currNgram)]
  return(paste(currNgram, collapse = " "))
}


