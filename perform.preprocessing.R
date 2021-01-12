#'Preprocessing of a text with intention to estimate sample size
#'
#'#'returns a vector of sentences with integer values as possible sample size specification
#'@param anAbstract a character vector
#'
#'@details
#'This function prepares an abstract text of a psychological study for sample size estimation.
#'Patterns with obviously non-relevant integer values are replaced by placeholder
#'
#'@export
#'
#'@examples
#'abstract <- "Methods: Our sample consisted of 1,250 aged between 18 and 65 years (sixty per cent female)."
#'perform.preprocessing(abstract)

perform.preprocessing <- function (anAbstract) {

# remove bibliographic information from text
#bibPattern <- "(\\(|[a-z](\\.)*, )((1|2)(0|9)[0-9][0-9](.|..|,|))+([^\\)])*\\)"
  bibPattern <- "\\((e\\.g\\.\\,? ?)?(([A-z] ?)+; )?([A-Z][a-z]+[^\\)]*\\, )?(1|2)(0|9)[0-9][0-9][a-z]?(\\, ([^\\)])+)?\\)"
  # remove capital letter shortcuts (like SDQ-20)
  shortcutPattern <- "([A-Z][A-Z]+( ?-? ?)|[A-z]+-)[0-9]+"
  # some key words with following numbers surely not sample size
  ex <-paste(get.exclusionKeys(), collapse = "| ")

  preprocessed <- gsub(shortcutPattern, " _REPSC_ ", anAbstract, ignore.case = FALSE)
  # hope that sample size doesn't stand in braces without any letter! next line is important for functioning bib-pattern
  preprocessed <- gsub("\\([0-9]+\\)", "_REP_", preprocessed)
  preprocessed <- gsub(bibPattern, " _REPBIB_ ", text2num(preprocessed), ignore.case = FALSE)
  #handle per cent, F(x,xx) (and other upcoming)
  preprocessed <-gsub("[0-9]+(\\.[0-9]+)? ?(\\%|per cent)", " _REPPC_ ", preprocessed, ignore.case = TRUE)
  preprocessed <-gsub("[A-Z]+ ?\\([0-9]\\, [0-9]+\\)", " _REPF_ ", preprocessed, ignore.case = FALSE)
  #das hier muss ich noch anders lösen!! Vielleicht thematisch alles was zu schule gehört behandeln
  preprocessed <-gsub("grade [0-9]+", "_REPG_", preprocessed, ignore.case = TRUE)
  preprocessed <-gsub("in (1|2)(0|9)[0-9][0-9]( and (1|2)(0|9)[0-9][0-9])?", " _REPYEAR_ ", preprocessed, ignore.case = TRUE )
  # remove ranges and age specifications (i.e. 1 to 20; between 18 and 70, 3-6, ...)
  # (kann ich evt. noch vereinfachen / zusammenfassen)
  rangePattern <- paste("( |\\()[0-9]+( | ?- ?| to )[0-9]+","( ",ex,")?", sep = "")
  agePattern1 <- "(between|from)? [0-9]+ (and|to) [0-9]+ (days?|weeks?|months?|years?)"
  agePattern2 <- "age.? (of )?[0-9]+"
  enumPattern <- paste("([0-9]+-?\\, )+and [0-9]+", sep = "")
  preprocessed <- gsub(rangePattern, " _REPRANGE_ ", preprocessed, ignore.case = TRUE)
  preprocessed <- gsub(enumPattern, " _REPENUM_ ", preprocessed, ignore.case = TRUE)
  preprocessed <- gsub(agePattern1, " _REPAGE_ ", preprocessed, ignore.case = TRUE)
  preprocessed <- gsub(agePattern2, " _REPAGE_ ", preprocessed, ignore.case = TRUE)
  # return all remaining sentences with integer values
  return(get.sentences.with.integer(preprocessed))
}
