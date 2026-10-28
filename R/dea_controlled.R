#' Drug controlled substance synonyms
#'
#' A dataset containing the controlled substances, and their other names, from
#' the DEA Orange Book "Controlled Substances - Alphabetical Order" list
#' (edition dated 21-Oct-2026). It was made by
#' `inst/scripts/extract_dea_orange.R`. Substances without other names are
#' not included.
#'
#' @docType data
#' @keywords datasets
#' @usage data(dea_controlled)
#' @format A tibble with 803 rows and 5 variables:
#' \describe{
#'   \item{substance}{formal drug name}
#'   \item{number}{DEA controlled substance code number (`NA` for some
#'     schedule V preparations)}
#'   \item{schedule}{drug schedule number}
#'   \item{narcotic}{Yes No indicator if the drug is a narcotic}
#'   \item{synonym}{synonym name}
#' }
#' @source <https://www.deadiversion.usdoj.gov/schedules/orangebook/c_cs_alpha.pdf>
#'
"dea_controlled"
