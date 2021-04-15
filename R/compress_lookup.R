
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
#' @param sortOutput Sort alphabetically rows of the returned table. Defaults
#'   to FALSE (maintaining the row order of the original \code{lookupTable}
#'   object).
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
  # browser()

  colsToKeep_lgl <- !c(compressOriginalWord, compressClass, compressCategory, compressSynonym)
  collapsed_df <- unique(
    lookupTable[, colsToKeep_lgl, drop = FALSE]
  )

  if (sortOutput) {

    # I don't know how many columns collapsed_df will have, so I can't pass all
    #   the columns to order(). TBH, I don't know why this works.
    out <- collapsed_df[do.call(order, as.list(collapsed_df)), , drop = FALSE]

  } else {
    out <- collapsed_df
  }

  row.names(out) <- NULL
  out

}
