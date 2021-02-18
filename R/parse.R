#######Function to parse free text to single vector of drugs-------------------
##I: path to data file
##O: vector of drugs
##read in data from mei-chen-----
library(readxl)
drug_data <- read_xlsx(here::here("./inst/extdata/CTN94TEXTFILLS.xlsx"))

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
library(tidytext)
library(dplyr)
data("stop_words")
drug_stop_words <- c("a", "few", "mg", "pills", "pill","days", "off", "bunch", "street",
                     "tab", "tabs", "detox", "rx", "not", "unsure", "unknown",
                     "clinic", "bottle", "unknkwn", "type", "patch", "pm", "which",
                     "injection", "er", "medication")
#example data
# cases <- tibble(drug = c("Ambien,Bup/Nx", "Bup/Nx.", "bup/nx", "Bup Nx.", "Bup Xx.",
#                           "BupNx", 'heroin - "few days on, few days off"',
#                           "Ambien = 2 pills", 'Ambien "a bunch" = 2 pills',
#                           "promethazine (25mg), clonidine (0.1mg)", "8 mg street suboxone",
#                           "street suboxone", "neurotin, Suboxone, 	street suboxone",
#                           "Seroquel ambien neurotin"))

##INTERACTIVE - must respond!
drugs <- get_col()
drugCol <- as.name(names(drugs))

#unnest tokens and remove special characters
unnested_drugs <- drugs %>%
  unnest_tokens(word, drugCol, token = "regex", pattern = "[, | \\s+]",
                to_lower = TRUE) %>%
  anti_join(stop_words) %>%
  filter(!grepl("[0-9]|[=]|[-]|[&]|[ \\s+]", word))
#filter for drug specific stop-words
filtered_drugs <- gsub('"',"", unnested_drugs$word) %>%
  tibble(drug = .) %>%
  #filter custom stopwords
  filter(!drug %in% drug_stop_words)

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
  tibble(drug = .) %>%
  na.omit()

####Test---------------------

freq <- janitor::tabyl(clean_names$drug)

#with lookup()
library(DOPE)
test <- clean_names$drug[1:10]
DOPE::lookup(test)

