
#' Parse a vector of free text containing drug information
#'
#' @description This function provides a dataframe of parsed out strings from a
#'   free text field, input as a vector, specified by the user.
#'
#' @param drug_vec A vector containing the free text to be parsed
#'
#' @return A n x 1 vector of class \code{character}.
#' @importFrom utils data
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' parse(drug_vec)
#' }

parse <- function(drug_vec){

  #binding vars to function
  word <- drug <- for_token <- NULL

  ####Parse out drug names-------------------------------
  #convert vector to df
  drugs <- as.data.frame(drug_vec)

  #unnest tokens and remove special characters
  #get special cases
  #ex. Bup/Nx = Bup/Nx. = bup/nx = Bup Nx. = Bup Nx. = Bup Xx. = BupNx
  ## should be preserved - replace with any with spaces to have / so all
  ## are consistently bup/nx
  #ex. and combination of "speedball" also preserved
  unnested_drugs <- drugs %>%
    tidytext::unnest_tokens(word, drug_vec, token = "regex", pattern = "[,|-]",
                            to_lower = TRUE) %>%
    dplyr::mutate(drug = trimws(tolower(word)),
                  #logic if a word can be tokenized or must remain as is
                  is_token = stringr::str_detect(drug, "(?=.*bup)(?=.*nx|.*nal|.*nar)|(?=.*speed)(?=.*ball)"),
                  #get the word to be tokenized
                  for_token =  dplyr::case_when(stringr::str_detect(drug, "(?=.*bup)(?=.*nx|.*nal|.*nar)") == TRUE ~ "bup/nx",
                                                stringr::str_detect(drug, "(?=.*speed)(?=.*ball)") == TRUE ~ "speedball",
                                                #change any "/" to spaces
                                                is_token == FALSE ~ gsub("/", " ", drug),
                                                TRUE ~ drug),
    ) %>%
    tidytext::unnest_tokens(word, for_token, token = "regex", pattern = "[ ]",
                            to_lower = TRUE) %>%
    dplyr::anti_join(tidytext::stop_words) %>%
    dplyr::filter(!grepl("[0-9]|[=]|[-]|[&]|[(]|[)]|[.]", word))

  #filter for drug specific stop-words
  filtered_drugs <- gsub('"',"", unnested_drugs$word)
  filtered_drugs <- dplyr::tibble(drug = filtered_drugs) %>%
    #filter custom stopwords
    dplyr::filter(!drug %in% drug_stop_words)

  #####Cleaning------------------------------
  # extra cleaning
  clean_names <- sapply(filtered_drugs$drug, function(x){
    #remove anything after a / or - or = or "
    name <- sub("\\(.*|-.*|=.*|\".*", NA, x)
    #Remove any strings that have parentheses and content
    sapply(name,
           function(x){
             #remove anything content within parentheses
             name <- trimws(gsub("\\([^\\)]+\\)","",x))
             #remove any extraneous characters
             #gsub("[^A-Za-z ]","",name)
           })
  })
  clean_names <- clean_names %>%
    unname() %>%
    na.omit()
  clean_names

}
