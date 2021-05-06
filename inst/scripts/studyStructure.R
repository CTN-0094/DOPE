library(DOPE)
library(dplyr)
library(stringr)


dea_brands <- DOPE::dea_brands %>%
  rename("brand" = brands) %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(brand = str_remove_all(brand, "®")) %>%
  mutate(brand = case_when(brand == "kadianms-contin" ~ "kadian", # two names
                           TRUE ~ brand)) %>%
  bind_rows(c(category = "morphine", brand = "ms contin"))


dea_class_cat <- DOPE::dea_factsheets %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(category =
           case_when(category == "ghb - gamma-hydroxybutyric acid" ~ "ghb",
                     category == "ecstasy or mdma (also known as molly)" ~ "mdma",
                     TRUE ~ category))

dea_street_names <- DOPE::dea_street_names %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(category =
           case_when(category == "amphetamine" ~ "amphetamines",
                     category == "fentanyl and fentanyl derivatives"	~ "fentanyl",
                     category == "mescaline"	~ "peyote and mescaline",
                     category == "peyote"	~ "peyote and mescaline",
                     TRUE ~ category))

categories <- data.frame(allCategory = c(dea_brands$category,
                                   dea_street_names$category,
                                   dea_class_cat$category)) %>%
  distinct() %>%
  arrange(allCategory)

library(sqldf)

# lookup table
lookup_df <- sqldf("select cc.class, a.category, a.syn synonym from
                (select b.category, b.brand as syn from dea_brands as b
                 union
                 select s1.category, s1.slang as syn from dea_street_names as s1
                 union
                 select s2.category, s2.brand as syn from dea_street_names as s2 where s2.brand <> NULL) as a
              left join dea_class_cat as cc on a.category = cc.category")

use_data(lookup_df, overwrite = TRUE)



# summary of category info
x <- sqldf("select distinct c.allCategory, b.Brand, s.Slang, cc.CategoryCat from categories as c left join
           (select category, 'Yes' as Brand from dea_brands) as b on b.category = c.allCategory
           left join
           (select category, 'Yes' as Slang from dea_street_names) as s on s.category = c.allCategory
           left join
           (select category, 'Yes' as CategoryCat from dea_class_cat) as cc on cc.category = c.allCategory" )


# the controleld file has capitals mixed in
dea_controlled <- DOPE::dea_controlled %>%
  mutate(across(where(is.character), tolower))


# lookup table with substance (problematic because all versions of fentanyl)
synPlusSubstance <- sqldf(
  "select b.*, c.substance from
  (select cc.class, a.category, a.syn from
    (select b.category, b.brand as syn from dea_brands as b
      union
     select s1.category, s1.slang as syn from dea_street_names as s1
      union
     select s2.category, s2.brand as syn from dea_street_names as s2 where s2.brand <> NULL
    ) as a
  left join dea_class_cat as cc on a.category = cc.category) as b
left join dea_controlled as c on b.syn = c.synonym or b.class=c.synonym")


