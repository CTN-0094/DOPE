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
  } else {
    print("your search matched multiple categories.")
    print("if you know which category you'd like to search select one below or choose all")
    categories <- match[!duplicated(match[,c('category')]),]
    print(categories)
    user_input <- readline(prompt="Enter row number of your match or 0 to search all: ")
    if (user_input == 0){
      answer = lookup(match$category, searchSynonym = FALSE)
    } else {
      answer = lookup(match[user_input,'category'], searchSynonym = FALSE)
    }
  }

  answer <- answer %>%
    rename("category_match" = category) %>%
    select(-original_word)
  answer
}



