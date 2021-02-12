#######Function to parse free text to single vector of drugs-------------------
##I: path to data file
##O: vector of drugs
##read in data from mei-chen-----
library(readxl)

drug_text <- read_xlsx("./inst/extdata/CTN94TEXTFILLS.xlsx")

###ID drug col----------------

cols <- colnames(drug_text)
colIndex <- seq(length(cols))
#gather all cols separated by a new line for readline
purrr::map_chr()


readline(paste("Which column contains drug names?: \n", colIndex[1], cols[1],
               "\n", "2. ", cols[2]))
