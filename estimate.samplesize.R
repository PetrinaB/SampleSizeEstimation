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

estimate.samplesize <- function(anAbstract = "", minmax = "max") {
  if(grepl("(case (study|report))|(report (of|the) case)", anAbstract))
    return(1)
  #Spezialfaelle - nur ganz wenige Personen !!
  if(grepl("2 (([a-z]+ ){,3})case studies", text2num(anAbstract))){ #!!! Hier so etwas wie 2 bis 5 individuals, students, participants einbauen und die Zahl (1...5) wiedergeben. 
    return(2)
  }
  # preprocessing of abstract an extraction of sentences with remaining integervalues
  sentences <- perform.preprocessing(anAbstract)
  # ********** estimation of samplesize **************
  
  # a) integer values smaller than 5 are ignored (have to change this!!)
  # b) when sample size is found in sentence -- break! (not in every study good idea)
  # c) somegthing like ' N = ... ', 'total of...', 'individuals', ' participants' ... are indicators
  # d) some key words indicate, that integer is no sample size information
  # have to multiply sample by 2 (sometimes this is not correct!!)
  doublePattern <- "dyads|couples|(sister|brother|father|mother(-| )infant) pairs" # maybe take 'pairs' and exclude 'fraction pairs'
  # vector where potential samplesize numbers are collected
  numVector <- c()
  subgroupFlag <- 0
  foundSample <- FALSE
  isMetaOrReview <- grepl("(meta( |-)?analysis)|((this|systematic|present|our|we) ([a-z]+ )?review)", anAbstract, ignore.case = TRUE)
  # if n = in Text, then only those numbers with n = are seen as sample
  onlyNcounts <- grepl("n ?= ?", anAbstract, ignore.case = TRUE)
  # if there's a distinction between 'N = ' and 'n = ', If 'N" ist mentioned, then 'n' is a subsample
  n_BigAndSmall <-  grepl("n ?= ?", anAbstract, ignore.case = FALSE) &&  grepl("N ?= ?", anAbstract, ignore.case = FALSE)
  #if multiple studies, there are two or more samples - decision necessary!!
  multipleStudies <- has.multipleStudies(anAbstract)
  # first pursue some simple strategies
  if(!multipleStudies && !onlyNcounts && !isMetaOrReview){
    if(length(sent <- simple.sample.sentences(sentences)) > 0)
      sentences <- sent
  }
#print(sentences)
  if(multipleStudies && !isMetaOrReview){
    #  print ("Mehrere Studien!")
    samp <- processMultipleStudies(anAbstract, minmax)
    if(samp > 0)
      return(samp)
  }
  #  if(isMetaOrReview){
  #    print("Metaanalyse/Review")
  #    print(anAbstract)
  #  }

  for(eachSentence in sentences){
    if(!foundSample || multipleStudies){
      #     remove last punctuation to get "clean" tokens
      theSentence <- substr(eachSentence, 1, nchar(eachSentence)-1)
      # ---------------------------------------------------------------
      # look for frequently used students sample. If sentence contains 
      # 'and', add to numVector else return amount of students
      # ---------------------------------------------------------------
      #      students <- get.student.sample.information(sentence)
      #      if(nchar(students) > 0){
      #        for(i in 1:5)
      #          students <- gsub("([0-9]+) ([A-z]+)", "\\1", students)
      #        print(students)
      #        if(!grepl("and", students))
      #          return(strtoi(students))
      #        re <- regexpr("[0-9]+",students)
      #        students <- substring(students,re,re+attr(re,"match.length"))
      #        numVector <- append(strtoi(students),numVector)
      #      }      
      # tokenize sentences (word as token)
      words <- unlist(vectorize.text(theSentence))
      for (index in 1:length(words)){
        token <- tolower(words[index])
        #clean the token
        token <- gsub(",|;|\\(|\\)","", token)
        # check whether token is integer value
        if(!is.na(num <- strtoi(token))){
          # look at some words before and after current number
          nGramBefore <- currentNgram(index, words, -4,0)
          # ich weiß nicht mehr, warum ich ein Kommma entferne - aber es hatte sicher einen Grund
          nGramBefore <- gsub(",", "", nGramBefore)
          nGramAfter <- currentNgram(index, words, 0,5)
          nPattern <- ifelse(n_BigAndSmall, paste("N ?= ?",num, sep = ""), paste("n ?= ?",num, sep = ""))
          factor <- ifelse(grepl(doublePattern, currentNgram(index, words, 0,7), ignore.case = TRUE),2, 1)
          # when something like "total of...", "total sample" is followed by num, then we found total sample size
          if(grepl("total ",nGramBefore, ignore.case = TRUE) && !multipleStudies){
            #            print(paste("Estimated samplesize: ",num*factor))
            return(num*factor)
          }
          currentIsN <- grepl(nPattern, nGramBefore, ignore.case = ifelse(n_BigAndSmall, FALSE, TRUE))
          if(onlyNcounts){ # if 'n = ' in text, then only numbers with 'n = ' count
            if(currentIsN)
              numVector <- append(numVector, num)
          } else {
            #            isPossibleSample <- !grepl("schools|communities|classes|classrooms|groups|days|locations|areas|domains|activities|paintings|pictures|images|photos?|organi(z|s)ations|types|categor(y|ies)|records|references|units|(sub)?tests|tasks|facets|factors|facts|studies", nGramAfter)
            isPossibleSample <- !grepl(get.notSampleTerms(), nGramAfter)
            # i.e. '100 participants, including 30 patients with ..."
            isSubgroup <- grepl(paste("includ(ing|es?|ed?) (([a-z]+ )+)?",num, sep = ""), nGramBefore) || (foundSample && grepl(paste("\\(",num, sep = ""),nGramBefore))
            subgroupFlag <- ifelse(isSubgroup, subgroupFlag + 1, subgroupFlag)
            if(isPossibleSample)
              numVector <- append(numVector, num)
          }
          #Hier ist zur Zeit festgelegt, dass ich nur Zahlen > 4 nehme. Ueberdenken!!!
          numVector <- numVector[numVector > 4]
        }
        foundSample <- length(numVector) > 0
      }
      if(multipleStudies){
        # this case is only relevant, if abstract cannot be devided into studies
        if(length(numVector > 0))
          numVector <- ifelse(minmax == "max", max(numVector), min(numVector))
      }
      #foundSample <- length(numVector > 0)
    }
  }
  if(length(numVector) == 0){
    #    print("Estimated samplesize: 0")
    return(0)
  }
  # found in an abstract: 78 individuals who made up 39 [...] couples

  if(factor > 1 && length(numVector) == 2 && (sort(numVector,decreasing = TRUE)[1] == sort(numVector,decreasing = TRUE)[2]*factor))
     return(numVector[1])
  numVector <- processNumbers(numVector, subgroupFlag) 
  #  print(paste("Estimated samplesize: ", numVector*factor))
  return(numVector*factor)  
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
  return(grepl("(study|experiment) 1\\,? |in the first (study|experiment)|(( |^)[0-9] (([A-z]+ ){,5})?(studies|experiments))", text2num(anAbstract), ignore.case = TRUE))
}

processMultipleStudies <- function(anAbstract, minmax = "max"){
  studVector <- unlist(strsplit2(anAbstract, "(([Ss]tud(y|ies)|[Ee]xperiments?) [0-9])|(([Ff]irst|[Ss]econd)( (study|experiment)|,))", "before"))
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

simple.sample.sentences <- function(sentenceVector){
  # find sentences with some often used sample references
  pattern1 <- "([Pp]articipants were [0-9]+( [a-z]+)+)"
  pattern2 <- "([0-9]+ ([A-z]+ )+(participated|were included))"
  pattern3 <- "[0-9]+ individuals"
  pattern4 <- "(sample (comprised|of) [0-9]+)"
  pattern5 <- "([0-9]+ ([A-z]+ ){0,4}students)"
  pattern6 <- "(data from [0-9]+)"
  thePattern <- paste(pattern1, "|", pattern2, "|", pattern3, "|", pattern4,"|", pattern5,"|", pattern6, sep = "")
  sentences <- unlist(lapply(sentenceVector, get.sentence.with.pattern, thePattern))
  sentences <- sentences[sentences>0]
  return(sentences)
}

get.student.sample.information <- function(aText){
  # returns pattern match like '100 high school students' or 'and 200 undergraduate students'
  # if no match, return value is empty char vector
  pattern <- "(and )?[0-9]+ ([A-z]+ ){0,4}students"
  reg <- regexpr(pattern, aText)
  
  return (substring(aText,reg,reg+attr(reg,"match.length")-1))
}

get.notSampleTerms <- function(){
  # returns pattern for words that are not kind of sample.
  # these words must not stand directly behind a number but also in the ngram after
  school <- "class(es|rooms)|schools|groups|"
  locations <- "areas|locations|communities|organi(z|s)ations|"
  study <- "studies|(sub)?tests|tasks|factors|dimensions|references|records|"
  grouping <- "domains|types|categor(y|ies)|units|facets|levels|"
  visual <- "images|paintings|photo(s|graphs)?|pictures|"
  time <- "days|"
  #last entry -> no OR
  other <- "activities|facts"
  return (paste(school, locations, study, grouping, visual, time, other, sep = ""))
}
