
#' Parse a vector of free text given a file path
#'
#' @description This function provides a dataframe of parsed out strings from a
#'   free text field given a file path specified by the user.
#'
#' @param path Path to the file containing the free text to be parsed
#'
#' @return A n x 1 dataframe with class \code{data.frame}, \code{tbl_df}, \code{tbl}.
#'   The dataframe has 1 variable, "drug"
#' @importFrom utils data
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#'   parse("./inst/extdata/CTN94TEXTFILLS.xlsx")

parse <- function(path){

  drug_data <- readxl::read_excel(path)

  ###ID drug col----------------

  cols <- colnames(drug_data)

  #gather all cols separated by a new line for readline
  colOptions <- purrr::map_chr(1:length(cols), function(.x){
    paste0(.x, ". ", cols[.x])
  })

  #function to subset data
  get_col <- function(){
    prompt <- stringr::str_c(c("Which column contains drug names?:",colOptions),
                             collapse = " \n")
    colNum <- as.numeric(readline(prompt))
    drug_data[ ,colNum] #returns tibble of n X 1
  }

  ####Parse out drug names-------------------------------
  stop_words <- data("stop_words", package = "tidytext")
  drug_stop_words <- c("a", "few", "mg", "pills", "pill","days", "off", "bunch", "street",
                       "tab", "tabs", "detox", "rx", "not", "unsure", "unknown",
                       "clinic", "bottle", "unknkwn", "type", "patch", "pm", "which",
                       "injection", "er", "medication", "mgs")

  ##INTERACTIVE - must respond!
  drugs <- get_col()
  drugCol <- as.name(names(drugs))

  #unnest tokens and remove special characters
  #special case: any combination of bup and nx should remain together
  unnested_drugs <- drugs %>%
    tidytext::unnest_tokens(word, drugCol, token = "regex", pattern = "[, | \\s+]",
                  to_lower = TRUE) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::filter(!grepl("[0-9]|[=]|[-]|[&]|[ \\s+]", word))
  #filter for drug specific stop-words
  filtered_drugs <- gsub('"',"", unnested_drugs$word) %>%
    dplyr::tibble(drug = .) %>%
    #filter custom stopwords
    dplyr::filter(!drug %in% drug_stop_words)

  #####Cleaning------------------------------
  #ex. Bup/Nx = Bup/Nx. = bup/nx = Bup Nx. = Bup Nx. = Bup Xx. = BupNx
  #need to remove punctuation
  clean_names <- purrr::map_chr(filtered_drugs$drug, function(.x){
    #remove anything after a / or - or = or "
    name <- sub("/.*|-.*|=.*|\".*", "", .x)
    #Remove any strings that have parentheses and content
    sapply(name,
           function(x){
             #remove anything content within parentheses
             name <- trimws(gsub("\\([^\\)]+\\)","",x))
             #remove any extraneous characters
             gsub("[^A-Za-z ]","",name)
           })
  }) %>%
    dplyr::tibble(drug = .) %>%
    na.omit()

}
