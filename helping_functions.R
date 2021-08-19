
# ************************* helping functions *************************************

# *****************************************************
# returns a vector with sentences with integer values
# digits may not be followed by letters or by full stop
# unless it's the end of line.
#******************************************************
get.sentences.with.integers <- function(aText) {
  text <- gsub("(=)([0-9])","\\1 \\2", aText)
  sentences <- unlist(JATSdecoder::text2sentences(text))
  intPattern <- "( |\\(|^)[0-9]+(,[0-9]+)?( |,|;|\\)|\\.$)"
  return(sentences[grep(intPattern, sentences)])
}

#**************************************************************
# helping function:returns a RegEx pattern with common measures
# like kg, cm, and hrs etc. 
#**************************************************************

get.measurePattern <- function(){
  #returns measurement units collapsed by | for regex-pattern search
  return (" ?(\\°|db|(milli)?sec(onds)?|(n|m)?s|min(utes)?|m?m|cm|km(/h)?|g|k?g(/m ?2)?|h(r|rs|z)?)( |\\)|,|;)")
}


#**************************************************************
# helping function: returns a RegEx pattern with all months like 
#**************************************************************
get.monthPattern <- function(){
  #returns all months collapsed by | for regex-pattern search
  return ("(january|february|march|april|may|june|july|august|september|october|november|december)")
}


#**************************************************************
# helping function: returns a RegEx pattern with an integer
# number followed by non sample words. This function is used by
# function perform.preprocessing().
#**************************************************************
get.exclusionKeyPattern <- function(){
  # indicators that an integer is not a sample size
  adj <- "main|specific|different|familiar|functional|public|private|"
  stud <- paste0("study|experiments?|tests?|articles|papers|analyses|",
                 "factors?|items?|tasks?|trials?|scales?|")
  time <- "times?|hours?|days?|weeks?|months?|years?|waves?|sessions?|"
  groups <- "groups?|schools|universities|communities|classrooms|rooms|classes|"
  other <- paste0("steps?|elements|objects|points?|records|databases|",
                  "categories|guidelines|concepts")
  return (paste0("[0-9]+ ","(",adj,stud,time,groups,other,")"))
}


#**************************************************************
# helping function: returns an n-gram of elements of aVector 
# around anIndex (size depends on lower- and upperLimit
# This function is used by function extimate.samplesize()
#**************************************************************
currentNgram <- function (anIndex, aVector, lowerLimit, upperLimit){
  currNgram <- c()
  for(i in lowerLimit:upperLimit){
    if(anIndex + i > 0)
      currNgram <- append(currNgram,aVector[anIndex + i])
  }
  currNgram <- currNgram[!is.na(currNgram)]
  return(paste(currNgram, collapse = " "))
}


