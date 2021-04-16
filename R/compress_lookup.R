
#' Collapse Redundant Rows of a Lookup Table
#'
#' @description Given a Drug Lookup table as returned by the function
#'   \code{\link{lookup}}, collapse rows from unwanted columns
#'
#' @param lookupTable A lookup table with category \code{data.frame} having three
#'   columns: drug class, drug category, and drug street name. These tables are
#'   returned by the function \code{\link{lookup}}.
#' @param compressOriginalWord Should the search word(s) be collapsed? Defaults to
#'   \code{FALSE}.
#' @param compressClass Should the drug class be collapsed? Defaults to
#'   \code{FALSE}.
#' @param compressCategory Should the drug category be collapsed? Defaults to
#'   \code{FALSE}.
#' @param compressSynonym Should the drug synonym / street name be collapsed?
#'   Defaults to \code{TRUE}.
#'
#' @return A compressed lookup table, with unwanted columns removed.
#'
#' @export
#'
#' @examples
#'   longExampleTable <- lookup("dope", "methamphetamine")
#'   compress_lookup(longExampleTable)
#'   compress_lookup(longExampleTable, compressCategory = TRUE)

compress_lookup <- function(lookupTable,
                            compressOriginalWord = FALSE,
                            compressClass = FALSE,
                            compressCategory = FALSE,
                            compressSynonym = TRUE,
                            sortOutput = FALSE) {

  colsToKeep_lgl <- !c(compressOriginalWord, compressClass, compressCategory, compressSynonym)
  collapsed_df <- unique(
    lookupTable[, colsToKeep_lgl, drop = FALSE]
  )

  row.names(collapsed_df) <- NULL
  collapsed_df

}
