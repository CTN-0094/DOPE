# Make package test data
# Layla Bouzoubaa
# 03/2021
# UPDATED: 20210506

library(readxl)
library(dplyr)
drug_data <- read_xlsx(here::here("./inst/extdata/test_parse/CTN94TEXTFILLS.xlsx")) %>%
  select(textdrug) %>%
  slice(1:500)

drug_df <- drug_data %>%
  mutate(sex = sample(c("male", "female"), nrow(drug_data),
                      replace = TRUE),
         race = sample(c("white", "black", "asian", "hn/pi", "ai/an", "hispanic"),
                       nrow(drug_data),replace = TRUE))

usethis::use_data(drug_df, overwrite = TRUE)
