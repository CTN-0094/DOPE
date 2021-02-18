
#' Make a table with the category and class for a drug name
#'
#' @description This function provides a table with drug category and class
#'   information all of the known drugs.
#'
#' @param ... multiple strings holding possible possible drug names
#' @param searchCategory Should the substances listed in \code{...} be searched
#'   for in column \code{category}? Defaults to TRUE.
#' @param searchClass Should the substances listed in \code{...} be searched
#'   for in column \code{class}? Defaults to TRUE.
#' @param searchSynonym Should the substances listed in \code{...} be searched
#'   for in column \code{synonym}? Defaults to TRUE.
#'
#' @return A lookup table with class \code{data.frame} having three columns:
#'   drug category, drug class, and drug street name.
#' @export
#'
#' @examples
#'   lookup("zip", "shrooms")

lookup <- function(...,
                   searchCategory = TRUE,
                   searchClass = TRUE,
                   searchSynonym = TRUE) {
  # browser()

  # Convert all names to lower case; https://github.com/labouz/DOPE/issues/39
  thingy_char <- vapply(
    X = as.character(as.list(...)),
    FUN = tolower,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  # Find rows that match the thingy, but set the base logic to FALSE
  categoryRowMatches <- classRowMatches <- synonymRowMatches <- FALSE
  if (searchCategory) {
    categoryRowMatches <- DOPE::lookup_df$category %in% thingy_char
  }
  if (searchClass) {
    classRowMatches <- DOPE::lookup_df$class %in% thingy_char
  }
  if (searchSynonym) {
    synonymRowMatches <- DOPE::lookup_df$synonym %in% thingy_char
  }

  # Combine row match logic (use OR for base FALSE layer)
  matches_lgl <- categoryRowMatches | classRowMatches | synonymRowMatches

  answer <-  DOPE::lookup_df[matches_lgl,, drop = FALSE]
  row.names(answer) <- NULL
  answer

}
