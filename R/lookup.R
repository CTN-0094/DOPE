
#' Make a table with the class and category for a drug name
#'
#' @description This function provides a table with drug class and category
#'   information all of the known drugs.
#'
#' @param drug_vec a vector of strings holding possible drug names
#' @param ... multiple strings holding possible drug names
#' @param searchClass Should the substances listed in \code{...} be searched
#'   for in column \code{class}? Defaults to TRUE.
#' @param searchCategory Should the substances listed in \code{...} be searched
#'   for in column \code{category}? Defaults to TRUE.
#' @param searchSynonym Should the substances listed in \code{...} be searched
#'   for in column \code{synonym}? Defaults to TRUE.
#' @param dropUnmatched Should words in \code{drug_vec} which do not match
#'   substances in our database be dropped from the table? Defaults to FALSE.
#'
#' @return A lookup table with category \code{data.frame} having four columns:
#'   original search term, drug class, drug category, and drug street name.
#'
#' @importFrom purrr map_df
#' @importFrom tidyr drop_na
#'
#' @export
#'
#' @examples
#'   lookup("zip", "shrooms")

lookup <- function(drug_vec = NULL, ...,
                   searchClass = TRUE,
                   searchCategory = TRUE,
                   searchSynonym = TRUE,
                   dropUnmatched = FALSE) {
  # browser()

  thingy <- c(drug_vec, as.character(list(...)))
  thingy_char <- vapply(
    X = as.character(thingy),
    FUN = tolower,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  # lookup individual words

  answer <- map_df(
    .x = thingy_char,
    .f = .lookup,
    searchClass,
    searchCategory,
    searchSynonym
  )

  row.names(answer) <- NULL

  if (dropUnmatched) {
    answer <- drop_na(answer, "class":"synonym")
  }

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

  answer <-  DOPE::lookup_df[matches_lgl, , drop = FALSE]

  if (nrow(answer) == 0){
    answer <- data.frame(
      original_word = x,
      class = NA_character_,
      category = NA_character_,
      synonym = NA_character_
    )
  } else{
    answer <- data.frame(
      original_word = x,
      answer
    )
  }

  answer

}
