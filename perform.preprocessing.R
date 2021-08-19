#'Preprocessing of a text with intention to estimate sample size
#'
#'#'returns a vector of sentences containing integer values as possible sample size specification
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
#'Methods: Our sample consisted of 1,250 scientists aged between 35 and 65 years (sixty per cent female)."
#'perform.preprocessing(abstract)

perform.preprocessing <- function (anAbstract) {
  
  # patterns to remove bibliographic information from text
  bibPattern1 <- "\\((e\\.g\\.\\,? ?)?(([A-z] ?)+; )?(([A-Z]\\. )+)?([A-Z][a-z]+[^\\)]*\\, )?(1|2)(0|9)[0-9][0-9][a-z]?(\\, ([^\\)])+)?\\)"
  bibPattern2 <- "[A-Z][a-z]+( et al\\.)?, (1|2)(0|9)[0-9][0-9][a-z]?\\)"
  # remove capital letter shortcuts (like SDQ-20)
  shortcutPattern <- "([A-Z][A-Z]+( ?-? ?)|([0-9]+)?[A-z]+-)[0-9]+( and [0-9]+)?|[0-9]+-" #das muss ich noch mal pruefen
  range1 <- "( |\\()[0-9]+-? ?(-|to) ?[0-9]+( |[,;-]|\\)|\\.)"
  range2 <- "(between|from) [0-9]+ (to|and) [0-9]+"
  rangePattern <- paste("(", range1, ")|(", range2, ")", sep = "")
  # remove likert explanation in braces like 1 (very good) to 5 (very bad), so that range can be recognized
  preprocessed <- gsub(" ?\\([^0-9]+\\)", "", anAbstract)
  # replace '5- ' or '20-' (have to replace this before text2num()!!)

  preprocessed <- gsub(rangePattern, " REPRANGE", anAbstract, ignore.case = TRUE)
  preprocessed <- gsub(shortcutPattern, " REPSC", preprocessed, ignore.case = FALSE)
  # hope that sample size doesn't stand in braces without any letter! next line is important for functioning bib-pattern
  preprocessed <- gsub("\\([0-9]+( and [0-9]+)?\\)", "REP", preprocessed)
  preprocessed <- gsub(bibPattern1, "REPBIB", preprocessed, ignore.case = FALSE)
  preprocessed <- gsub(bibPattern2, "REPBIB", preprocessed, ignore.case = FALSE)

  for(i in 1:2) # remove ',' from numbers (up to 9,999,999)
    preprocessed <- gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",preprocessed)  
  # replace enumerations
  enumPattern <- paste("([0-9]+-?\\, )+(and|or) [0-9]+", sep = "")
  preprocessed <- gsub(enumPattern, " REPENUM", text2num(preprocessed), ignore.case = TRUE)
  
  #handle per cent, F(x, xx) (and other upcoming)
  preprocessed <- gsub("[0-9]+(\\.[0-9]+)? ?(\\%|per )", "REPPC", preprocessed, ignore.case = TRUE)
  preprocessed <- gsub("[A-Z]+ ?\\([0-9]\\, [0-9]+\\)", "REPF", preprocessed, ignore.case = FALSE)
  preprocessed <- gsub("[0-9] [0-9]( [0-9])+", "_REP_NUMCHAIN", preprocessed)
  preprocessed <- gsub("[^Nn]( = |=)[0-9]+", "_REP_EQU", preprocessed)
  preprocessed <-gsub("[0-9]+ of [0-9]+", " _REP_xOFy", preprocessed)
  #das hier muss ich noch anders lösen!! Vielleicht thematisch alles was zu schule gehört behandeln
  preprocessed <-gsub("(version |grade |experiments? |stud(y|ies) )[0-9]+( (to |and )[0-9]+)?", "REPG", preprocessed, ignore.case = TRUE)
  #eliminate years
  preprocessed <-gsub("(in |after |before |since |until |till |during )([a-z]+ )?(year )?(1|2)(0|9)[0-9][0-9]( and (1|2)(0|9)[0-9][0-9])?", "REPYEAR", preprocessed, ignore.case = TRUE )
  preprocessed <-gsub("(< ?|> ?|than |over |under |at (least |most )?)[0-9]+", "REPCOMP", preprocessed, ignore.case = TRUE )
  preprocessed <-gsub("[0-9]+ (in|or|out of|vs.) [0-9]+", "REP_ OR_", preprocessed, ignore.case = TRUE ) 

  agePattern <- "( |\\()age(d|s)? (of )?([a-z]+ )?[0-9]+( and [0-9]+)?"
  preprocessed <- gsub(agePattern, " REPAGE_", preprocessed, ignore.case = TRUE)
  preprocessed <- removeDateInfo(preprocessed)
  preprocessed <- removeNumbersFollowedByExKey(preprocessed)
  preprocessed <- removeSubgroupDeclaration(preprocessed) 
  preprocessed <- gsub("[0-9]+\\-", " _REP", preprocessed)
  preprocessed <- gsub("(recruited from|identified|completed) [0-9]+ ", "REP_VERBS", preprocessed)
  preprocessed <- gsub("[Qq]uestionnaire ([A-z]+ )?[0-9]+", "REP_QUEST", preprocessed)
  preprocessed <- gsub(" (the(se)?|a|or|through|that|their|(out of)|(set of)|into|all) [0-9]+( |,|;|\\.|\\))", " _REPWORD REPNUMBER_", preprocessed)
  # return all remaining sentences with integer values
  return(get.sentences.with.integer(preprocessed))
}

removeNumbersFollowedByExKey <- function(aText){
  ex <- get.exclusionKeyPattern()
  meas <- paste("([0-9]+ (and |or |to ))?[0-9]+",get.measurePattern(), sep = "")
  text <- gsub(ex, "REPEXKEY ", aText, ignore.case = TRUE)
  text <- gsub(meas, "REPMEASURE", text, ignore.case = TRUE)
  return(text)
}

removeSubgroupDeclaration <- function(aText){
  # just one subgroup is named? Have to ignore this (sometimes there is also a percentage mentioned with has been replaced before)
  #hier muss noch ein negative lookahead hin (?!(boys|girls|males|...)), aber es funktioniert nicht recht
  
  #oder einfach typische Angaben (mit MAge und so auch mit beruecksichtigen)
  #subgroupPattern <- "\\([0-9]+ (women|girls|females?|men|boys|males?)([^\\)]+)?\\)"
  subgroupPattern <- "\\([0-9]+ (women|girls|females?|men|boys|males?)( \\(REPPC\\))?\\)" 
  text <- gsub(subgroupPattern, " REP_SUBGR", aText, ignore.case = FALSE)
  return(text)
}

removeDateInfo <- function(aText){
  "information like 'january, 31, 2021' or 'march 2001' or '22 february, 2000"
  year <- "[12][09][0-9][0-9]"
  day <- "[0-9]+(st|nd|rd|th)?"
  datePattern <- paste("(",day," )?",get.monthPattern(), "( ",day,")?", ",? (",year,")?", sep = "")
  text <- gsub(datePattern, " REP_DATE", aText, ignore.case = TRUE)
  text <- gsub(paste(year," until ","(",year,")?", sep = ""), "REP_DATE", text)
  return(text)
}

