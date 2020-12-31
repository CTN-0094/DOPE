
#' Collapse Redundant Rows of a Lookup Table
#'
#' @description Given a Drug Lookup table as returned by the function
#'   \code{\link{lookup}}, collapse rows from unwanted columns
#'
#' @param lookupTable A lookup table with class \code{data.frame} having three
#'   columns: drug category, drug class, and drug street name. These tables are
#'   returned by the function \code{\link{lookup}}.
#' @param compressCategory Should the drug category be collapsed? Defaults to
#'   \code{FALSE}.
#' @param compressClass Should the drug class be collapsed? Defaults to
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
#'   compress_lookup(longExampleTable, compressClass = TRUE)

compress_lookup <- function(lookupTable,
                            compressCategory = FALSE,
                            compressClass = FALSE,
                            compressSynonym = TRUE,
                            sortOutput = FALSE) {
  # browser()

  colsToKeep_lgl <- !c(compressCategory, compressClass, compressSynonym)
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
