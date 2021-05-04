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


# write_rds(everything, here::here("inst/extdata/IQVIA/iqvia_raw.rds"))

# parse() on iqvia data---------------------------------------------------------
iqvia <- read_rds("inst/extdata/IQVIA/iqvia_raw.rds")

parse_iqvia <- DOPE::parse(iqvia$what) %>%
  tibble() %>%
  distinct()

fill_lookup <- function(x){
  #browser()
  if(x %in% c("cd", "dhc", "codeine", "endocet", "hycd", "hydrocodone","hysingla",
              "ibudone", "lorcet", "lortab", "nalocet", "norco", "oxaydo",
              "oxycodone", "oxycontin", "percocet", "primlev", "roxicodone",
              "roxybond", "trezix", "vicodin", "vicoprofen", "xtampza",
              "zohydro", "xodol")){

     df <- tibble(class = "narcotics (opioids)",
           category = "codeine combinations, non-injectable",
           synonym = x)
  }
  else if(x %in% c("acamprosate",  "antabuse",
              "bunavail" ,     "buprenex",
              "bup/nx",        "buprenorphine",
              "butrans",       "disulfiram",
              "naltrexone",    "probuphine" ,
              "sublocade",     "suboxone",
              "vivitrol",      "zubsolv")){
    df <- tibble(class = "treatment drug",
           category = "treatment drug",
           synonym = x)
  }
  else if(x %in% c("apap", "fiorinal", "ascomp", "butalb", "asa", "ibuprofen",
              "fioricet", "tylenol", "ibuprof")){
    df <- tibble(class = "analgesic",
           category = "pain relief",
           synonym = x)
  }
  else if(x %in% c("dilaudid", "fentanyl", "hydromorphone", "infumorph", "morphine",
              "nalbuphine")){
    df <- tibble(class = "narcotics (opioids)",
           category = "morphine-opium, injectable",
           synonym = x)
  }
  else if(x %in% c("abstral", "actiq", "arymo", "belbuca", "duragesic",
              "embeda", "exalgo","fentora","kadian","lazanda","morphabond",
              "opana", "opium","oxymorphone","subsys", "ms")){
    df <- tibble(class = "narcotics (opioids)",
           category = "morphine-opium, non-injectable",
           synonym = x)
  }
  else if(x %in% c("evzio", "naloxone", "narcan")){
    df <- tibble(class = "reversal agent",
           category = "reversal agent",
           synonym = x)
  }
  else if(x %in% c("butorphanol", "demerol", "meperidine", "methadone")){
    df <- tibble(class = "narcotics (opioids)",
           category = "synthetic narcotic, injectable",
           synonym = x)
  }
  else if(x %in% c("conzip", "dolophine", "levorphanol", "methadose",
              "nucynta", "pentazocine", "naloxo", "tramadol", "ultracet",
              "ultram")){
    df <- tibble(class = "narcotics (opioids)",
           category = "synthetic narcotic, non-injectable",
           synonym = x)
  }
  else if(x %in% c("caf")){
    df <- tibble(class = "diuretic",
                 category = "caffeine",
                 synonym = x)
  }else{
    df <- tibble(class = "WHAT",
                 category = "WHAT",
                 synonym = x)
  }

  df
}

# drug_vec <- parse_iqvia$drug %>%
#   unname()

iqvia <- map_dfr(parse_iqvia$., fill_lookup)

usethis::use_data(iqvia, overwrite = TRUE)
