##### Add IQVIA data to lookup table--------------------------------------------

library(conflicted)
suppressMessages(conflict_prefer("filter", "dplyr"))
suppressPackageStartupMessages(library(tidyverse))

suppressMessages(library(gsubfn))
suppressMessages(library(readxl))
library(sqldf)

#Get data from each sheet and bind----------------------------------------------
#"Codeine & Comb, non-injectable"
CCNI <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                  sheet = "Codeine & Comb, non-injectable",
                  col_names = c("what", "count"), skip = 1)
#"Drug Dependence"
DD <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                sheet = "Drug Dependence",
                col_names = c("what", "count"), skip = 1)
#"Morphine-Opium, Injectable"
MOI <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                 sheet = "Morphine-Opium, Injectable",
                 col_names = c("what", "count"), skip = 1)
#"Morphine-Opium, Non Injectable"
MONI <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                  sheet = "Morphine-Opium, Non Injectable",
                  col_names = c("what", "count"), skip = 1)
#"Opioid Rev agts"
ORA <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                 sheet = "Opioid Rev agts",
                 col_names = c("what", "count"), skip = 1)
#"Synth Narcotic, Injectable"
SNI <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                 sheet = "Synth Narcotic, Injectable",
                 col_names = c("what", "count"), skip = 1)
#"Synth Narcotic, Non-Injectable"
SNNI <- read_xlsx(here::here("inst/extdata/IQVIA","Copy of OVERALL_CATEGORIES-4.xlsx"),
                  sheet = "Synth Narcotic, Non-Injectable",
                  col_names = c("what", "count"), skip = 1)

everything <- tibble(what = unique(c(CCNI$what, DD$what, MOI$what, MONI$what, ORA$what, SNI$what, SNNI$what)))


write_rds(everything, here::here("inst/extdata/IQVIA/iqvia.rds"))

# parse() on iqvia data---------------------------------------------------------
iqvia <- read_rds("inst/extdata/IQVIA/iqvia.rds")

parse_iqvia <- parse(iqvia$what)

fill_lookup <- function(x){
  browser()
    if(x %in% c("cd", "dhc", "codeine", "endocet", "hycd", "hydrocodone","hysingla",
                   "ibudone", "lorcet", "lortab", "nalocet", "norco", "oxaydo",
                   "oxycodone", "oxycontin", "percocet", "primlev", "roxicodone",
                   "roxybond", "trezix", "vicodin", "vicoprofen", "xtampza",
                   "zohydro")){

      tibble(category = "narcotics (opioids)",
             class = "Codeine & Comb, non-injectable")
    }else{
      tibble(category = "blah",
             class = "blah")
    }
  }

class_iqvia <- parse_iqvia %>%
  map_df(.$drug, fill_lookup)
