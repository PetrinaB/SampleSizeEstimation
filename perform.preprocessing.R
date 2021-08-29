#'Preprocessing of a text with intention to estimate sample size.
#'
#'returns a vector of sentences containing integer values as possible sample size specifications.
#'
#'@param anAbstract a character vector
#'
#'@details
#'This function prepares an abstract text of a psychological study in English language for sample size extraction
#'Patterns with obviously non-relevant integer values are replaced by placeholders
#'
#'@export
#'
#'@examples
#'abstract <- "Objectives: The aim of this study was to get an impression of sample sizes.
#'Methods: Our sample consisted of 1,250 scientists aged between 35 and 65 years (six per cent female)."
#'perform.preprocessing(abstract)

perform.preprocessing <- function (anAbstract) {
  # patterns to remove bibliographic information from text
  bibPattern1 <- "\\((e\\.g\\.\\,? ?)?(([A-z] ?)+; )?(([A-Z]\\. )+)?([A-Z][a-z]+[^\\)]*\\, )?(1|2)(0|9)[0-9][0-9][a-z]?(\\, ([^\\)])+)?\\)"
  bibPattern2 <- "[A-Z][a-z]+( et al\\.)?, (1|2)(0|9)[0-9][0-9][a-z]?(\\)| ?;)"
  # remove capital letter shortcuts (like SDQ-20)
  shortcutPattern <- "([A-Z][A-Z]+( ?-? ?)|([0-9]+)?[A-z]+-)[0-9]+( and [0-9]+)?|[0-9]+-" #das muss ich noch mal pruefen
  range1 <- "( |\\()[0-9]+-? ?(-|to) ?[0-9]+( |[,;-]|\\)|\\.)"
  range2 <- "(between|from) [0-9]+ (to|and) [0-9]+"
  rangePattern <- paste0("(", range1, ")|(", range2, ")")
  # remove likert explanation in braces like 1 (very good) to 5 (very bad), so that range can be recognized
  preprocessed <- gsub(" ?\\([^0-9]+\\)", "", anAbstract)
  # replace '5- ' or '20-' (have to replace this before text2num()!!)
  preprocessed <- gsub(rangePattern, " REP _RANGE", anAbstract, ignore.case = TRUE)
  preprocessed <- gsub(shortcutPattern, " REP _SC", preprocessed, ignore.case = FALSE)
  # hope that sample size doesn't stand in braces without any letter! next line is important for functioning bib-pattern
  preprocessed <- gsub("\\([0-9]+( and [0-9]+)?\\)", "REP_BRCS", preprocessed)
  preprocessed <- gsub(bibPattern1, " REP _BIB", preprocessed, ignore.case = FALSE)
  preprocessed <- gsub(bibPattern2, " REP _BIB", preprocessed, ignore.case = FALSE)

  for(i in 1:2) # remove ',' from numbers (up to 9,999,999)
    preprocessed <- gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",preprocessed)
  # replace enumerations
  preprocessed <- gsub("\\[|\\]", "", preprocessed)
  enumPattern <- paste0("([0-9]+-?\\, )+(and|or) [0-9]+")
  preprocessed <- gsub(enumPattern, " REP _ENUM", JATSdecoder::text2num(preprocessed), ignore.case = TRUE)
  #handle per cent, F(x, xx) (and other upcoming)
  preprocessed <- gsub("[0-9]+(\\.[0-9]+)? ?(\\%|per )", " REP _PER ", preprocessed, ignore.case = TRUE)
  preprocessed <- gsub("[A-Z]+ ?\\([0-9]\\, [0-9]+\\)", " REP _F ", preprocessed, ignore.case = FALSE)
  preprocessed <- gsub("[0-9] [0-9]( [0-9])+", " REP _NUMCHAIN ", preprocessed)
  preprocessed <- gsub("[^Nn]( = |=)[0-9]+", " REP _EQU ", preprocessed)
  preprocessed <-gsub("[0-9]+ of [0-9]+", " REP _xOFy ", preprocessed)
  #eliminate some leading words and numbers like 'grade 6 and 7' or 'experiment 2 to 5'
  preprocessed <-gsub("(version |grade |experiments? |[Ff]ormula |stud(y|ies) )[0-9]+( (to |and )[0-9]+)?", " REP _LEAD ", preprocessed, ignore.case = TRUE)
  #eliminate years
  preprocessed <-gsub("(in |after |before |since |until |till |during )([a-z]+ )?(year )?(1|2)(0|9)[0-9][0-9]( and (1|2)(0|9)[0-9][0-9])?", " REP _YEAR", preprocessed, ignore.case = TRUE )
  preprocessed <-gsub("(< ?|> ?|than |over |under |at (least |most )?)[0-9]+", " REP_COMP ", preprocessed, ignore.case = TRUE )
  preprocessed <-gsub("[0-9]+ (in|or|out of|vs.) [0-9]+", " REP _OR ", preprocessed, ignore.case = TRUE )

  agePattern <- "( |\\()age(d|s)? (of )?([a-z]+ )?[0-9]+( and [0-9]+)?"
  preprocessed <- gsub(agePattern, " REP _AGE ", preprocessed, ignore.case = TRUE)
  preprocessed <- removeDateAndTimeInfo(preprocessed)
  preprocessed <- removeMeasure(preprocessed)
  preprocessed <- removeSubgroupDeclaration(preprocessed)
  preprocessed <- gsub("[0-9]+\\-", " REP _NUM_HyPHEN", preprocessed)
  preprocessed <- gsub("[0-9]+ (possible|main|specific|different|familiar|functional|public|private)", "REP _ADJS", preprocessed)
  preprocessed <- gsub("(recruited from|identified|completed) [0-9]+ ", "REP _VERBS", preprocessed)
  preprocessed <- gsub("(from|at) [0-9]+ ([a-z]+ )?(schools|universities|hospitals)", " REP _FROM", preprocessed)
  preprocessed <- gsub("[Qq]uestionnaire ([A-z]+ )?[0-9]+", " REP _QUEST", preprocessed)
  preprocessed <- gsub(" (the(se)?|a|or|through|that|their|(out of)|(set of)|into|all) [0-9]+( |,|;|\\.|\\))", " _REP _WORD_AND_NUMBER_", preprocessed)
  # return all remaining sentences with integer values
  return(get.sentences.with.integers(preprocessed))
}

removeMeasure <- function(aText){
  meas <- paste ("([0-9]+ (and |or |to ))?[0-9]+",get.measurePattern())
  text <- gsub(meas, "REP _MEASURE", aText, ignore.case = TRUE)
  return(text)
}

removeSubgroupDeclaration <- function(aText){
  # just one subgroup is named? Have to ignore this (sometimes there is also a percentage mentioned with has been replaced before)
  subgroupPattern <- "\\([0-9]+ (women|girls|females?|men|boys|males?)( \\(REP_PC \\))?\\)"
  text <- gsub(subgroupPattern, " REP _SUBGR", aText, ignore.case = FALSE)
  return(text)
}

removeDateAndTimeInfo <- function(aText){
  "information like 'january, 31, 2021' or 'march 2001' or '22 february, 2000"
  year <- "[12][09][0-9][0-9]"
  day <- "[0-9]+(st|nd|rd|th)?"
  datePattern <- paste0("(",day," )?",get.monthPattern(), "( ",day,")?", ",? (",year,")?")
  text <- gsub(datePattern, " REP _DATE", aText, ignore.case = TRUE)
  text <- gsub(paste0(year," until ","(",year,")?"), " REP _DATE", text)
  "information like '20 weeks' or 'a 6 month follow up'"
  text <- gsub(" [0-9]+ (times|hours|days|weeks|months|years|waves)",
               " REP _TIME", text, ignore.case = TRUE)
  return(text)
}

###################################################################################
#  This function returns a vector of sentences which contain an integer value
###################################################################################

get.sentences.with.integers <- function(aText) {
  sentences <- unlist(JATSdecoder::text2sentences(aText))
  intPattern <- "( |\\(|^)[0-9]+(,[0-9]+)?( |,|;|\\)|\\.$)"
  return(sentences[grep(intPattern, sentences)])
}
###################################################################################
#  function to build a simple regex pattern with several measures collapsed by '|'
###################################################################################

get.measurePattern <- function(){
  return ("(db|(milli)?sec(onds)?|(n|m)?s|min(utes)?|(m|c|k)?m(/h)?|g|k?g(/m ?2)?|h(r|rs|z)?)( |\\)|,|;)")
}

#**************************************************************
# helping function: returns a RegEx pattern with all months like
#**************************************************************
get.monthPattern <- function(){
  #returns all months collapsed by | for regex-pattern search
  return ("(january|february|march|april|may|june|july|august|september|october|november|december)")
}


