
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
#'   lookup("zip")

lookup_syn <- function(drug_name) {

  # Make sure drug_name is a string
  if (is.string(drug_name)){
    drug_name <- tolower(drug_name)
  } else {
    stop("drug_name should be a string of one drug name")
  }

  # lookup individual words
  match <- lookup(drug_name, searchClass=FALSE,
                          searchCategory=FALSE,
                          searchSynonym=TRUE)

  row.names(answer) <- NULL
  answer

}

# internal function to look a single "word"
# takes a vector of length one hold the word to lookup and returns a
#   data frame with all matches plus the original word

.lookup <- function(x,
                    searchClass,
                    searchCategory,
                    searchSynonym) {

  # Find rows that match the thingy, but set the base logic to FALSE
  classRowMatches <- categoryRowMatches <- synonymRowMatches <- FALSE
  if (searchClass) {
    classRowMatches <- DOPE::lookup_df$class %in% x
  }
  if (searchCategory) {
    categoryRowMatches <- DOPE::lookup_df$category %in% x
  }
  if (searchSynonym) {
    synonymRowMatches <- DOPE::lookup_df$synonym %in% x
  }

  # Combine row match logic (use OR for base FALSE layer)
  matches_lgl <- classRowMatches | categoryRowMatches | synonymRowMatches

  answer <-  DOPE::lookup_df[matches_lgl,, drop = FALSE]

  if (nrow(answer) == 0){
    answer <- data.frame(original_word = x, class = NA_character_,  category = NA_character_,
                         synonym = NA_character_)
  } else{
    answer <- data.frame(original_word = x, answer)
  }
  answer
}
