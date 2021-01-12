#'Estimation of sample size
#'
#'returns an estimated sample size given in the abstract text of a psychological study
#'@param text a character vector
#'
#'@details
#'This function estimates the sample size mentioned in the abstract of a posychological study. 
#'If there's no indication of a sample size, return value is 0
#'
#'@export
#'
#'@examples
#'theText <- c("In this study, a total sample of 333 children was asked about their hobbies. One hundred were boys and 233 were girls.")
#'estimate.samplesize(theText)


if(!require("JATSdecoder")){
  setwd("C://Petra//Psychologie//U H H//Masterarbeit//JATSdecoder//")
  install.packages("JATSdecoder")
  library("JATSdecoder")
}



# dokumentation mit dem package "roxy2gen"
# nicht vergessen!! Exception handling!! (package available)

estimate.samplesize <- function(anAbstract) {

  # is 'n = ' or 'N = ' is named in text? (sometimes it's also (n 123))
  nPattern <- "( |\\()n (=|[0-9]+)"
  n <- grepl(nPattern,anAbstract, ignore.case = TRUE)
  # preperation of the text
  sentences <- perform.preprocessing(anAbstract)
print(sentences)
  # ********** estimation of samplesize **************
  
  # some key words that indicate, that integer is no sample size information
  exclusionKeys <- get.exclusionKeys()
  doublePattern <- "dyads|couples|(sister|brother|mother infant) pairs" # in einer studie war die Rede von fraction pairs!
  simplePattern <- "participants (are|were) |sample of [0-9]+( [A-z]+)*\\." #das ist wohl eher doof
  
  numVector <- c()
  multipleStudies <- FALSE
  totalSample <- 0

  for(each in sentences){
    #remove brackets and last punctuation to get "clean" tokens
    sentence <- gsub("\\(|\\)", " _rep_ ", each)
    sentence <- substr(sentence, 1, nchar(sentence)-1)
    if(grepl("(study 1|experiment 1)(\\,? |$)|first (study|experiment)|([0-9] ([A-z]+ )+(studies|experiments))", sentence, ignore.case = TRUE)) #prüfen, ob die patterns so gut sind
      multipleStudies <- TRUE #wird später behandelt!!
    words <- unlist(vectorize.text(sentence))
    
    # ich gehe hier die Wörter durch und gucke, ob evt. eine Maßeinheit ODER SO dahinter oder davor steht.
    # wenn nicht, nehme ich die Zahl auf.
    # schlauer: das ganze als pattern finden und löschen?
    
    for (index in 1:length(words)){
      
      token <- tolower(words[index])
      succ <- ifelse(index == length(words), token, words[index+1])
      pred <- ifelse(index == 1, token, words[index-1])
      token <- gsub(",|;","", token)
      succ <- tolower(gsub(",|\\.|;","", succ))
#      pred <- tolower(gsub(",|\\.|;","", pred))
     
      if(!is.na(num <- strtoi(token)) && !contains.element(exclusionKeys, succ) && !contains.element(exclusionKeys, pred) && !is.measurementUnit(succ) && !is.month(pred) && !is.month(succ)){
        nGramBefore <- currentNgram(index, words, -4,0)
        nGramAfter <- currentNgram(index, words, 0,8)
# print (paste("Number = : ", token))
        currentIsN <- grepl(nPattern, nGramBefore, ignore.case = TRUE)
# factor 2 for dyads, couples, ... 
        factor <- ifelse(grepl(doublePattern, nGramAfter, ignore.case = TRUE),2, 1)
        if(n){ # if 'n = ' in text, then only numbers with 'n = ' count
          if(currentIsN)
            numVector <- append(numVector, num)
        }else{
          if(grepl(simplePattern, nGramBefore, ignore.case = TRUE))
            return(print(num * factor))
          numVector <- append(numVector, num)
        }

          if(grepl("total",nGramBefore, ignore.case = TRUE) && !multipleStudies)
          return(print(num*factor)) 
      }
    } 
    if(multipleStudies){
      print("Mehrere Studien!")
# Hier gebe ich zur Zeit das Maximum aus, habe aber noch keine Teilstichprobenberechnung dabei!!
      numVector <- ifelse(length(numVector) == 0, 0, max(numVector))
    }
  }
  if(length(numVector) == 0)
    return(print(0))
  #das hier ist jetzt eine krücke!!! (siehe unten)
  numVector <- processNumbers(numVector) 
  print(paste("Estimated samplesize: ", numVector*factor))
  return(numVector*factor)  
}

get.exclusionKeys <- function(){
  exVector <- c("study", "experiment", "wave", "waves", "time", "times", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years", "session", "sessions", "item", "items", "factor", "factors", "questionnaire", "inventory")
  return(exVector)
}

processNumbers <- function(aNumVector){

  if((len <- length(aNumVector)) == 1)
    return(aNumVector)
  first <- aNumVector[1]
  rest <- aNumVector[2:len]
  aNumVector <- aNumVector[aNumVector > 9]
  return(ifelse(first == sum(rest)&&(len > 2), first, ifelse(sum(aNumVector) == 0,0, sum(aNumVector))))
}
  

# bisher noch keine Behandlung von  Meta-Analysen!!!

