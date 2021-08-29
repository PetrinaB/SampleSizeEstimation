#'estimate.samplesize
#'
#'estimation of sample size mentioned in the abstract of a psychological study
#'
#'returns an integer value as sample size estimation
#'
#'@param anAbstract a string or character vector with the abstract of an article
#'@param minmax a parameter to decide for multiple experiments or studies whether the
#'smallest or the largest sample will be returned #'possible values are "min" and "max"
#'with "max" as default value
#'
#'@details
#'This function estimates the sample size mentioned in the abstract of a posychological study.
#'Some heuristics help to identify sentences with a sample information, some calculations lead
#'to an overall sample, if sub samples are mentioned.
#'If there's no indication of a sample size, return value is 0
#'
#'@export
#'
#'@examples
#'text_1 <- c("In this study, a total sample of 333 children was asked about their hobbies. One hundred were boys and 233 were girls.")
#'text_2 <- "We collected data from 200 female and 160 male students."
#'text_3 <- "In study 1, we investigated N=250 baseball players. In study 2,N=355 female baseball players are investigated."
#'estimate.samplesize(text_1)
#'estimate.samplesize(text_2)
#'estimate.samplesize(text_3, "max")

# ********** estimation of samplesize ***********************************************
# a) found samples smaller than 5 are ignored (--> case studies)
# b) when sample size is found in sentence -- break!
# c) somegthing like ' N = ... ', 'total of...', 'individuals', ' participants' ... are indicators
# d) some key words indicate, that integer is no sample size information (exclusion keys)
# ************************************************************************************
estimate.samplesize <- function(anAbstract = "", minmax = "max") {
  if(is.null(anAbstract))
    return(0)
  # preprocessing of abstract returns an extraction of sentences with remaining integer values
  sentences <- perform.preprocessing(anAbstract)
  # have to multiply sample by 2 if doublePattern. Some 'pairs' are not samples (e.g. 'fraction pairs')
  doublePattern <- "dyads|couples|(sister|brother|father|mother(-| )infant) pairs"
  # the vector where potential sample size numbers are collected
  numVector <- c()
  subgroupFlag <- 0
  # turns to TRUE if a sentence contains a sample size
  foundSample <- FALSE
  isMetaOrReview <- grepl("(meta( |-)?analysis)|((this|systematic|present|our|we) ([a-z]+ )?review)", anAbstract, ignore.case = TRUE)
  # if n = in Text, then only those numbers with n = are seen as sample
  onlyNcounts <- grepl("n ?= ?", anAbstract, ignore.case = TRUE)
  # if there's a distinction between 'N = ' and 'n = ', If 'N" ist mentioned, then 'n' is a subsample
  n_BigAndSmall <-  grepl("n ?= ?", anAbstract, ignore.case = FALSE) &&  grepl("N ?= ?", anAbstract, ignore.case = FALSE)
  #if multiple studies, there are two or more samples - (decision by parameter minmax)
  multipleStudies <- has.multipleStudies(anAbstract)
  # first pursue some simple strategies
  if(!multipleStudies && !onlyNcounts && !isMetaOrReview){
    if(length(sent <- simple.sample.sentences(sentences)) > 0)
      sentences <- sent
  }
  if(multipleStudies && !isMetaOrReview){
    samp <- processMultipleStudies(anAbstract, minmax)
    if(samp > 0)
      return(samp)
  }
  for(eachSentence in sentences){
    if(!foundSample || multipleStudies){
      # remove last punctuation to get "clean" tokens
      theSentence <- substr(eachSentence, 1, nchar(eachSentence)-1)
      # tokenize sentences (word as token)
      words <- unlist(JATSdecoder::vectorize.text(theSentence))
      for (index in 1:length(words)){
        token <- tolower(words[index])
        #clean the token
        token <- gsub(",|;|\\(|\\)","", token)
        # check whether token is integer value
        if(!is.na(num <- strtoi(token))){
          # look at some words before and after current number
          nGramBefore <- currentNgram(index, words, -4,1)
          nGramBefore <- gsub(",", "", nGramBefore)
          nGramAfter <- currentNgram(index, words, 0,3)
          nPattern <- ifelse(n_BigAndSmall, paste0("N ?= ?",num), paste0("n ?= ?",num))
          factor <- ifelse(grepl(doublePattern, theSentence, ignore.case = TRUE),2, 1)
          isPossibleSample <- !grepl(get.exclusionKeys(), nGramAfter)
          # if found something like "total of...", "total sample" is followed by num, then it seems to be total sample size
          if(grepl("total ",nGramBefore, ignore.case = TRUE) && !multipleStudies && isPossibleSample)
            return(num*factor)
          # look wheter n/N = comes before current integer
          currentIsN <- grepl(nPattern, nGramBefore, ignore.case = ifelse(n_BigAndSmall, FALSE, TRUE))
          if(onlyNcounts){ # if 'n = ' in text, then only numbers with 'n = ' count
            if(currentIsN)
              numVector <- append(numVector, num)
          } else {
            isPossibleSample <- !grepl(get.exclusionKeys(), nGramAfter)
            # e.g. '100 participants, including 30 patients with ..."
            isSubgroupType1 <- grepl(paste("including (([a-z]+ )+)?",num, sep = ""), nGramBefore)
            # e.g. '100 students (40 female)...
            isSubgroupType2 <- (foundSample && grepl(paste("\\(",num," [a-z]+\\)", sep = ""),nGramBefore))
            # e.g. "250 participants from 30 communities"
            isFromTerm <- (foundSample && grepl(paste("(from |across |at )",num," [a-z]+", sep = ""),nGramBefore))
            subgroupFlag <- ifelse(isSubgroupType1, subgroupFlag + 1, subgroupFlag)
            if(isPossibleSample && !isSubgroupType2 && !isFromTerm)
              numVector <- append(numVector, num)
          }
          #only samples greater than 4 count
          numVector <- numVector[numVector > 4]
        }
        foundSample <- length(numVector) > 0
      }
      if(multipleStudies){
        # this case is only relevant, if abstract text cannot be devided into studies
        if(length(numVector > 0))
          numVector <- ifelse(minmax == "max", max(numVector), min(numVector))
      }
    }
  }
  if(length(numVector) == 0){
    # no sample size mentioned or sample size smaller than 5
    return(0)
  }
  # handle special case found in an abstract: 78 individuals who made up 39 [...] couples
  if(factor > 1 && length(numVector) == 2 && (sort(numVector,decreasing = TRUE)[1] == sort(numVector,decreasing = TRUE)[2]*factor))
    return(numVector[1])
  # process found numbers:
  estimation <- processNumbers(numVector, subgroupFlag)
  return(estimation*factor)
}

processNumbers <- function(aNumVector, aFlag){
  # if aNumVector contains only one number, nothing to process (return aNumVector)
  # else:
  #   if first number equals sum of all other numbers, return first number (the other
  #   numbers seem to be  reported subgroups
  #   if aFlag is set, only a part of subgroups is reported. Return first number
  #   else: add all numbers up.
  if((len <- length(aNumVector)) == 1)
    return(aNumVector)
  first <- aNumVector[1]
  rest <- aNumVector[2:len]
  subGroupsReported <- aFlag > 0
  return(ifelse(first == sum(rest)&&(len > 2), first, ifelse(subGroupsReported, first, sum(aNumVector))))
}

has.multipleStudies <- function(anAbstract){
  return(grepl("(study|experiment) 1\\,? |in the first (study|experiment)|(( |^)[0-9] (([A-z]+ ){,5})?(studies|experiments))", JATSdecoder::text2num(anAbstract), ignore.case = TRUE))
}

processMultipleStudies <- function(anAbstract, minmax = "max"){
  studVector <- unlist(JATSdecoder::strsplit2(anAbstract, "(([Ss]tud(y|ies)|[Ee]xperiments?) [0-9])|(([Ff]irst|[Ss]econd)( (study|experiment)|,))", "before"))
  if(length(studVector) < 2)
    return(0)
  numVector <- c()
  start <- ifelse(grepl(anAbstract, pattern = "[Ss]tudy 1[;\\.\\)]"), 1,2)
  #print(start)
  #print(studVector)
  for(index in start:length(studVector)){
    each <- gsub("([Ss]tudy|studies|[Ee]xperiments?)( [0-9] ?(to|-|and) ?[0-9])?","REP_STUD", studVector[index])
    s <- estimate.samplesize(each)
    numVector <- append(numVector,s)
  }
  if(minmax == "max")
    numVector <- sort(numVector, decreasing = TRUE)
  if(minmax == "min")
    numVector <- sort(numVector, decreasing = FALSE)
  return(ifelse(length(numVector) > 0, numVector[1], 0))
}
###################################################################################
#  this function return an n-gram around an element of aVector -
#  n-gram boundaries are the given lower- and upperLimit around anIndex
###################################################################################

currentNgram <- function (anIndex, aVector, lowerLimit, upperLimit){
  currNgram <- c()
  for(i in lowerLimit:upperLimit){
    if(anIndex + i > 0)
      currNgram <- append(currNgram,aVector[anIndex + i])
  }
  currNgram <- currNgram[!is.na(currNgram)]
  return(paste(currNgram, collapse = " "))
}


simple.sample.sentences <- function(sentenceVector){
  # find sentences with some often used sample references
  pattern1 <- "([Pp]articipants were [0-9]+( [a-z]+)+)"
  pattern2 <- "([0-9]+ ([A-z]+ )+(participated|were included))"
  pattern3 <- "[0-9]+ individuals"
  pattern4 <- "(sample (comprised|of) [0-9]+)"
  pattern5 <- "([0-9]+ ([A-z]+ ){0,4}students)"
  pattern6 <- "(data (was |were )?(drawn |collected )?from [0-9]+)"
  pattern7 <- "(A total of [0-9]+)"
  thePattern <- paste(pattern1, "|", pattern2, "|", pattern3, "|", pattern4,"|", pattern5,"|", pattern6,"|", pattern7, sep = "")
  sentences <- unlist(lapply(sentenceVector, JATSdecoder::get.sentence.with.pattern, thePattern))
  sentences <- sentences[sentences>0]
  return(sentences)
}

#***********************************************************************************************
# this function provides the fundamental list of key words to recognize non-relevant integers
# all 'exclusion keys' are put together as a regex pattern to filter them with pattern matching.
# Add further keys seperated by '|' to supplement this list.
# **********************************************************************************************

get.exclusionKeys <- function(){
  # returns pattern for words that are not kind of sample.
  # these words must not always be located directly behind a number but
  # also in the ngram after
  education <- "class(es|rooms)|schools|groups|universities|"
  locations <- "rooms|areas|locations|communities|organi(z|s)ations|hospitals|"
  study <- "studies|experiments|tasks|references|articles|papers|records|databases|"
  psych <- "trials|sessions|analyses|"
  testconstr <- "scales|items?|factors?|(sub)?tests|dimensions|guidelines|"
  grouping <- "domains|types|categor(y|ies)|units|facets|levels|blocks|"
  audioVisual <- "images|paintings|photo(s|graphs)?|pictures|tones?|"
  time <- "days|decades|"
  #last entry -> no OR-Pipe
  other <- "activities|facts|elements|objects|points?|steps?|concepts"
  return (paste0(education, locations, study, psych, testconstr, grouping, audioVisual, time, other))
}


