#######Function to parse free text to single vector of drugs-------------------
##I: path to data file
##O: vector of drugs
##read in data from mei-chen-----
library(readxl)

drug_text <- read_xlsx(here::here("./inst/extdata/CTN94TEXTFILLS.xlsx"))

###ID drug col----------------

cols <- colnames(drug_text)

#gather all cols separated by a new line for readline
colOptions <- purrr::map_chr(1:length(cols), function(.x){
  paste0(.x, ". ", cols[.x])
})

#function to subset data
get_col <- function(){
  prompt <- stringr::str_c(c("Which column contains drug names?:",colOptions),
                           collapse = " \n")
  colNum <- as.numeric(readline(prompt))
  drug_text[ ,colNum] #returns tibble of n X 1
}

####Parse out drug names
drugs <- get_col()


