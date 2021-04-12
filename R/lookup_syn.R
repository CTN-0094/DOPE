library(tidyverse)
#' Make a table with the class and category for a drug name
#'
#' @description This function provides a table with drug synonyms that have the
#'   same class and category as the search term.
#'
#' @param drug_name a string of a single drug name.
#'
#' @return A lookup table with category \code{data.frame} having four columns:
#'   original search term, drug class, drug category, and synonym name.
#' @export
#'
#' @examples
#'   lookup_syn("zip")

lookup_syn <- function(drug_name) {

  # Make sure drug_name is a string
  if (is.character(drug_name)){
    drug_name <- tolower(drug_name)
  } else {
    stop("drug_name should be a string of one drug name")
  }

  # lookup individual words
  match <- lookup(drug_name, searchClass=FALSE,
                          searchCategory=FALSE,
                          searchSynonym=TRUE)

  if (length(match$category) == 1){
    answer <- lookup(match$category, searchSynonym = FALSE)
    answer <- answer %>%
      rename("category_match" = category) %>%
      select(-original_word)
  } else {
    print("Your search matched multiple categories. Please choose one and refine your search")
    answer <- match
    answer <- answer %>%
      rename("match" = synonym)
  }
  answer
}



