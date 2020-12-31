
# add note to use singular instead of plural
# add note to use common abbreviations (lsd) vs long names

library(conflicted)
library(DOPE)
library(dplyr)
conflict_prefer("filter", "dplyr")
library(stringr)
library(tidyr) # pivot_longer()

# use this to find the drug names (and misspellings)
#ns <- data.frame(description = unique(tolower(DOPE::noslang_street_names$description)))

# use this to make analysis file
ns <- noslang_street_names %>%
  mutate(description = tolower(description))


checkForDrugs <- ns %>%
  mutate(d_2cb = as.numeric(str_detect(description, "2cb|nexus"))) %>%          # NS
  mutate(d_alphaEt = as.numeric(str_detect(description,
                                           "alpha-ethyltryptamine"))) %>%       # NS
  mutate(d_alprazolam = as.numeric(str_detect(description, "xanax"))) %>%
  mutate(d_amphetamine = as.numeric(str_detect(description,
                                               "amphetamine|speed"))) %>%
  mutate(d_amt = as.numeric(str_detect(description,
                                               "alpha-methyltryptamine"))) %>%  # NS
  mutate(d_amobarbital = as.numeric(str_detect(description,
                                       "amobarbital"))) %>% # NS
  mutate(d_amylNitrite = as.numeric(str_detect(description,
                                               "amyl nitrite"))) %>%            # NS
  mutate(d_barbiturates = as.numeric(str_detect(description,
                                                "barbiturate"))) %>%
  mutate(d_bathSalts  = as.numeric(str_detect(description, "bath salts"))) %>%
  mutate(d_benzodiazepines = as.numeric(str_detect(description,
         "benzodiazepine|benzodiazipines"))) %>%
  mutate(d_clonazepam = as.numeric(str_detect(description, "klonopin"))) %>%
  mutate(d_cocaine = as.numeric(str_detect(description,
         "cocaine|coke|coccaine"))) %>%
  mutate(d_codeine = as.numeric(str_detect(description, "codeine"))) %>%
  mutate(d_crack = as.numeric(str_detect(description, "crack"))) %>%
  mutate(d_dextromethorphan = as.numeric(str_detect(description,
        "dextromethorphan|coricidin|cortison"))) %>%                            # NS
  mutate(d_diazepam = as.numeric(str_detect(description, "valium"))) %>%        # NS
  mutate(d_dmt = as.numeric(str_detect(description, "dimethyltryptamine"))) %>% # NS
  mutate(d_fentanyl = as.numeric(str_detect(description, "fentanyl"))) %>%
  mutate(d_flakka = as.numeric(str_detect(description, "flakka"))) %>%
  mutate(d_gbl = as.numeric(str_detect(description, "gbl"))) %>%                # NS
  mutate(d_ghb = as.numeric(str_detect(description,
                                       "ghb|gamma hydroxybutyrate"))) %>%
  mutate(d_heroin = as.numeric(str_detect(description, "heroin|herion"))) %>%
  mutate(d_hydrocodone = as.numeric(str_detect(description,
         "hydrocodone|vicodin|lortab|loratab"))) %>%
  mutate(d_hydromorphone = as.numeric(str_detect(description,
         "hydromorphone|diluadid"))) %>%
  mutate(d_inhalants = as.numeric(str_detect(description, "inhalant"))) %>%
  mutate(d_isobutylNitrite = as.numeric(str_detect(description,
                                                   "isobutyl nitrite"))) %>%    # NS
  mutate(d_ketamine = as.numeric(str_detect(description, "ketamine"))) %>%
  mutate(d_khat = as.numeric(str_detect(description, "khat"))) %>%
  mutate(d_kratom = as.numeric(str_detect(description, "kratom"))) %>%
  mutate(d_lsd = as.numeric(str_detect(description,
        "lsd|lysergic acid diethylamide"))) %>%
  mutate(d_marijuana = as.numeric(str_detect(description,
         "marijuana|marijuna|cannabis|marajuana|weed|marijauna|maihuana|cannibus|hashish|hasish|blunt|tetrahydrocannabinol|joint|panama red"))) %>%
  mutate(d_mdma = as.numeric(str_detect(description,
                                        "mdma|ecstacy|ecxtasy|ecstasy"))) %>%
  mutate(d_mescaline = as.numeric(str_detect(description,
                                             "peyote|mescaline"))) %>%
  mutate(d_methadone = as.numeric(str_detect(description, "methadone"))) %>%
  mutate(d_methamphetamine = as.numeric(str_detect(description,
         "methamphetamine|crystal myth|crystal rock of meth|methamphetimine|crystal meth"))) %>%
  mutate(d_methcathinone = as.numeric(str_detect(description,
                                                 "methcathinone"))) %>%         # NS
  mutate(d_methaqualone = as.numeric(str_detect(description,
                                                "methaqualone"))) %>%           # NS
  mutate(d_methylphenidate = as.numeric(str_detect(description, "ritalin"))) %>%
  mutate(d_morphine = as.numeric(str_detect(description,
                                            "morphine|morophine"))) %>%
  mutate(d_mushrooms = as.numeric(str_detect(description, "mushroom"))) %>%
  mutate(d_nitrous = as.numeric(str_detect(description, "nitrous oxide"))) %>%  # NS
  mutate(d_opium = as.numeric(str_detect(description, "opium"))) %>%
  mutate(d_oxycodone = as.numeric(str_detect(description,
                                             "oxycodone|oxycontin|oxycotin"))) %>%
  mutate(d_pcp = as.numeric(str_detect(description, "pcp|phencyclidine"))) %>%  # capitalization needs to match dea_factsheets_plus
  mutate(d_psilocybin = as.numeric(str_detect(description, "psilocybin"))) %>%
  mutate(d_rohypnol = as.numeric(str_detect(description, "rohypnol"))) %>%
  mutate(d_salviaDivinorum = as.numeric(str_detect(description,
                                                   "salvia divinorum"))) %>%
  mutate(d_spice = as.numeric(str_detect(description, "spice"))) %>%
  mutate(d_steroids = as.numeric(str_detect(description,
                                            "steroids|steriods|steroid"))) %>%
  mutate(d_u47700 = as.numeric(str_detect(description, "u-47700")))  %>%
  rowwise() %>%
  mutate(known = sum(c_across(starts_with("d_")))) # %>%
  # use this for development
  # select(description, known, everything())










ns_drugs <- checkForDrugs %>%
  filter(known > 0) %>%
  select(-known) %>%
  mutate(description =
           case_when(description == "nexus" ~ "2cb",
                   description == "speed" ~ "amphetamine",
                   description == "benzodiazipines" ~ "benzodiazipine",
                   description == "coke" ~ "cocaine",
                   description == "coccaine" ~ "cocaine",
                   description == "coricidin" ~ "dextromethorphan",
                   description == "cortison" ~ "dextromethorphan",
                   description == "gamma hydroxybutyrate" ~ "ghb",
                   description == "vicodin" ~ "hydrocodone",
                   description == "lortab" ~ "hydrocodone",
                   description == "loratab" ~ "hydrocodone",
                   description == "herion" ~ "heroin",
                   description == "lysergic acid diethylamide" ~ "lsd",
                   description == "marijuna" ~ "marijuana",
                   description == "cannabis" ~ "marijuana",
                   description == "marajuana" ~ "marijuana",
                   description == "weed" ~ "marijuana",
                   description == "marijauna" ~ "marijuana",
                   description == "maihuana" ~ "marijuana",
                   description == "cannibus" ~ "marijuana",
                   description == "hashish" ~ "marijuana",
                   description == "hasish" ~ "marijuana",
                   description == "blunt" ~ "marijuana",
                   description == "tetrahydrocannabinol" ~ "marijuana",
                   description == "joint" ~ "marijuana",
                   description == "panama red" ~ "marijuana",
                   description == "ecstacy" ~ "mdma",
                   description == "ecxtasy" ~ "mdma",
                   description == "ecstasy" ~ "mdma",
                   description == "peyote" ~ "mescaline",       # need to fix in DEA
                   description == "crystal myth" ~ "methamphetamine",
                   description == "crystal rock of meth" ~ "methamphetamine",
                   description == "crystal meth" ~ "methamphetamine",
                   description == "methamphetimine" ~ "methamphetamine",
                   description == "morophine" ~ "morphine",
                   description == "oxycontin" ~ "oxycodone",
                   description == "oxycotin" ~ "oxycodone",
                   description == "phencyclidine" ~ "pcp",
                   description == "steriods" ~ "steroid",
                   description == "steroids" ~ "steroid",
                   TRUE ~ description))

# don't double count crack as both crack and cocaine (use crack)
# remove cocaine if "crack cocaine"
ns_drugs$d_cocaine[ns_drugs$d_crack > 0] <- 0

# don't double count meth as both meth and amphetamine (use meth)
# remove amphetamine if methamphetamine
ns_drugs$d_amphetamine[ns_drugs$d_methamphetamine > 0] <- 0

# don't triple count mdma as both meth and amphet (use mdma)
# remove amphetamine methamphetamine if methylenedioxymethamphetamine
ns_drugs$d_amphetamine[ns_drugs$d_mdma > 0] <- 0
ns_drugs$d_methamphetamine[ns_drugs$d_mdma > 0] <- 0

long <- ns_drugs %>%
  pivot_longer(cols=starts_with("d_"),
               names_to = "drug",
               values_to = "values",
               names_prefix = "d_") %>%
  filter(values > 0) %>%
  mutate(drug = case_when(drug == "alphaEt" ~ "alpha-ethyltryptamine",
                          drug == "amylNitrite" ~ "amyl nitrite",
                          drug == "bathSalts" ~ "bath salts",
                          drug == "isobutylNitrite" ~ "isobutyl nitrite",
                          drug == "nitrous" ~ "nitrous oxide",
                          drug == "salviaDivinorum" ~ "salvia divinorum",
                          TRUE ~ drug)) %>%
  mutate(street_name = str_remove(street_name ,"\\(spanish\\)")) %>%
  filter(! street_name %in% c("are you anywhere?"))




