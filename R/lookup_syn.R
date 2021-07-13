#' Make a table with the class and category for a drug name
#'
#' @description This function provides a table with drug synonyms that have the
#'   same class and category as the search term.
#'
#' @param drug_name a string of a single drug name.
#'
#' @return A lookup table with category \code{data.frame} having three columns:
#'   drug class, drug category match, and synonym name.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select filter
#'
#' @export
#'
#' @examples
#'   lookup_syn("zip")

lookup_syn <- function(drug_name) {
  # browser()

  #binding vars to function
  category <- original_word <- synonym <- NULL

  # Make sure only a single word is supplied (this function is not designed to
  #   be vectorised)
  if (length(drug_name) > 1L) {
    drug_name <- drug_name[1]
    warning("Only the first element of drug_name will be used.
  If you have multiple substances to query, please use lookup() instead.")
  }

  # Make sure drug_name is a string
  if (is.character(drug_name)){

    # make lowercase and remove leading and trailing whitespace
    drug_name <-
      tolower(drug_name) %>%
      trimws()

  } else {
    stop("drug_name should be a string of one drug name")
  }

  # lookup individual words
  matches_df <- DOPE::lookup(
    drug_name,
    searchClass = TRUE,
    searchCategory = TRUE,
    searchSynonym = TRUE
  )

  drugCateg_char <- matches_df[ , "category", drop = TRUE]
  cat_matches <- unique(drugCateg_char)

  if (length(cat_matches) == 1) {

    answer <- lookup(
      matches_df$category,
      searchClass = TRUE,
      searchCategory = TRUE,
      searchSynonym = FALSE
    )

    answer <- answer %>%
      rename("category_match" = category) %>%
      select(-original_word) %>%
      filter(synonym != drug_name)

  } else if (drug_name %in% drugCateg_char) {

    answer <- matches_df %>%
      filter(category == drug_name) %>%
      rename("category_match" = category) %>%
      select(-original_word) %>%
      filter(synonym != drug_name)

  } else {

    message(
"Your search matched multiple categories. Please choose one \n",
"from the following list and refine your search. Example: \n",
"lookup_syn('", cat_matches[1], "') or ",
"lookup_syn('", cat_matches[2], "')"
)

    answer <- unique(matches_df["category"])

  }

  answer

}



