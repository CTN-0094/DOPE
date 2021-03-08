
#' Make a table with the category and class for a drug name
#'
#' @description This function provides a table with drug category and class
#'   information all of the known drugs.
#'
#' @param drug_vec a vector of strings holding possible drug names
#' @param ... multiple strings holding possible drug names
#' @param searchCategory Should the substances listed in \code{...} be searched
#'   for in column \code{category}? Defaults to TRUE.
#' @param searchClass Should the substances listed in \code{...} be searched
#'   for in column \code{class}? Defaults to TRUE.
#' @param searchSynonym Should the substances listed in \code{...} be searched
#'   for in column \code{synonym}? Defaults to TRUE.
#'
#' @return A lookup table with class \code{data.frame} having four columns:
#'   original search term, drug category, drug class, and drug street name.
#' @export
#'
#' @examples
#'   lookup("zip", "shrooms")

lookup <- function(drug_vec = NULL, ...,
                   searchCategory = TRUE,
                   searchClass = TRUE,
                   searchSynonym = TRUE) {

  if (length(drug_vec) > 1){
    # we expect ... to be empty
    if(length(list(...)) > 0){
      stop("Using `drug_vec` argument with other words is not allowed. Please see the examples.", call. = FALSE)
    }
    # Convert all names to lower case; https://github.com/labouz/DOPE/issues/39
    thingy_char <- vapply(
      X = as.character(drug_vec),
      FUN = tolower,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  } else {
    # Convert all names to lower case; https://github.com/labouz/DOPE/issues/39
    thingy <- c(drug_vec, as.character(list(...)))
    thingy_char <- vapply(
      X = as.character(thingy),
      FUN = tolower,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  }

  # lookup individual words

  answer <- purrr::map_df(thingy_char, .lookup,
                          searchCategory,
                          searchClass,
                          searchSynonym)

  row.names(answer) <- NULL
  answer

}

# internal function to look a single "word"
# takes a vector of length one hold the word to lookup and returns a
#   data frame with all matches plus the original word

.lookup <- function(x,
                    searchCategory,
                    searchClass,
                    searchSynonym) {

  # Find rows that match the thingy, but set the base logic to FALSE
  categoryRowMatches <- classRowMatches <- synonymRowMatches <- FALSE
  if (searchCategory) {
    categoryRowMatches <- DOPE::lookup_df$category %in% x
  }
  if (searchClass) {
    classRowMatches <- DOPE::lookup_df$class %in% x
  }
  if (searchSynonym) {
    synonymRowMatches <- DOPE::lookup_df$synonym %in% x
  }

  # Combine row match logic (use OR for base FALSE layer)
  matches_lgl <- categoryRowMatches | classRowMatches | synonymRowMatches

  answer <-  DOPE::lookup_df[matches_lgl,, drop = FALSE]

  if (nrow(answer) == 0){
    answer <- data.frame(original_word = x, category = NA_character_,  class = NA_character_,
                         synonym = NA_character_)
  } else{
    answer <- data.frame(original_word = x, answer)
  }
  answer
}
