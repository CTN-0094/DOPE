
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
#' @importFrom stringr str_detect str_replace_all
#' @importFrom tidytext unnest_tokens
#' @importFrom dplyr mutate filter anti_join case_when
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#'   parse("Lortab and Percocet")
#'

parse <- function(drug_vec){

  # browser()

  #binding vars to function
  word <- drug <- for_token <- NULL
  drug_stop_words <- DOPE::"drug_stop_words"

  ####Parse out drug names-------------------------------
  #unnest tokens and remove special characters
  #get special cases
  #ex. Bup/Nx = Bup/Nx. = bup/nx = Bup Nx. = Bup Nx. = Bup Xx. = BupNx
  ## should be preserved - replace with any with spaces to have / so all
  ## are consistently bup/nx
  #ex. and combination of "speedball" also preserved
  unnestedDrugs_df <-
    tibble(drug_vec) %>%
    unnest_tokens(
      word, drug_vec, token = "regex", pattern = "[,|-|+|&]", to_lower = TRUE
    ) %>%
    mutate(
      drug = trimws(tolower(word))
    ) %>%
    # logic if a word can be tokenized or must remain as is
    mutate(
      is_token = str_detect(
        drug, "(?=.*bup)(?=.*nx|.*nal|.*nar)|(?=.*speed)(?=.*ball)"
      )
    ) %>%
    # get the word to be tokenized
    mutate(
      for_token = case_when(
        str_detect(drug, "(?=.*bup)(?=.*nx|.*nal|.*nar)") == TRUE ~ "bup/nx",
        str_detect(drug, "(?=.*speed)(?=.*ball)") == TRUE ~ "speedball",
        #change any "/" to spaces
        is_token == FALSE ~ gsub("/", " ", drug),
        TRUE ~ drug
      )
    ) %>%
    unnest_tokens(
      word, for_token, token = "regex", pattern = "[ ]", to_lower = TRUE
    ) %>%
    # dplyr::filter(!grepl("[0-9]|[=]|[-]|[&]|[(]|[)]|[.]", word))
    # The MS Word "smart quotes" are Unicode U+201C and U+201D
    mutate(
      word = str_replace_all(
        word, '[0-9]|[=]|[(]|[)]|[.]|["]|[\\u201C]|[\\u201D]', ' '
      ),
      word = trimws(word)
    ) %>%
    filter(word != "") %>%
    anti_join(tidytext::stop_words) %>%
    filter(!(word %in% drug_stop_words))


  # #filter for drug specific stop-words
  # filteredDrugs_char <- gsub('"',"", unnestedDrugs_df$word)
  # filteredDrugs_df <-
  #   dplyr::tibble(drug = filteredDrugs_char) %>%
  #   #filter custom stopwords
  #   dplyr::filter(!drug %in% drug_stop_words)



  #####Cleaning------------------------------
  # extra cleaning
  clean_names <- sapply(unnestedDrugs_df$word, function(x){
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
  clean_names <-
    clean_names %>%
    unname() %>%
    na.omit()
  clean_names

}
