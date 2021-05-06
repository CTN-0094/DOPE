#' Make a table with the class and category for a drug name
#'
#' @description This function provides a table with drug synonyms that have the
#'   same class and category as the search term.
#'
#' @param drug_name a string of a single drug name.
#'
#' @return A lookup table with category \code{data.frame} having three columns:
#'   drug class, drug category match, and synonym name.
#' @export
#'
#' @examples
#'   lookup_syn("zip")

lookup_syn <- function(drug_name) {

  #binding vars to function
  category <- original_word <- synonym <- NULL

  # Make sure drug_name is a string
  if (is.character(drug_name)){
    # make lowercase and remove leading and trailing whitespace
    drug_name <- tolower(drug_name) %>% trimws()
  } else {
    stop("drug_name should be a string of one drug name")
  }

  # lookup individual words
  matches <- DOPE::lookup(drug_name, searchClass=TRUE,
                          searchCategory=TRUE,
                          searchSynonym=TRUE)

  cat_matches <- unique(matches[,c("category")])

  if (length(cat_matches) == 1){
    answer <- lookup(matches$category, searchClass=TRUE,
                     searchCategory=TRUE,
                     searchSynonym = FALSE)
    answer <- answer %>%
      dplyr::rename("category_match" = category) %>%
      dplyr::select(-original_word)
    answer <- subset(answer, synonym != drug_name)
  } else if(drug_name %in% matches[,c("category")] ){
    answer <- matches[matches$category == drug_name, ]
    answer <- answer %>%
      dplyr::rename("category_match" = category) %>%
      dplyr::select(-original_word)
    answer <- subset(answer, synonym != drug_name)
  } else {
    print("Your search matched multiple categories. Please choose one from the following list and refine your search. Example: lookup_syn('amphetamine')")
    answer <- unique(matches[c("category")])
  }

  answer
}



