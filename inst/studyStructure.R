library(DOPE)
library(dplyr)
library(stringr)


dea_brands <- DOPE::dea_brands %>%
  rename("brand" = brands) %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(brand = str_remove_all(brand, "®")) %>%
  mutate(brand = case_when(brand == "kadianms-contin" ~ "kadian", # two names
                           TRUE ~ brand)) %>%
  bind_rows(c(class = "morphine", brand = "ms contin"))


dea_class_cat <- DOPE::dea_factsheets %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(class =
           case_when(class == "ghb - gamma-hydroxybutyric acid" ~ "ghb",
                     class == "ecstasy or mdma (also known as molly)" ~ "mdma",
                     TRUE ~ class))

dea_street_names <- DOPE::dea_street_names %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(class =
           case_when(class == "amphetamine" ~ "amphetamines",
                     class == "fentanyl and fentanyl derivatives"	~ "fentanyl",
                     class == "mescaline"	~ "peyote and mescaline",
                     class == "peyote"	~ "peyote and mescaline",
                     TRUE ~ class))

classes <- data.frame(allClass = c(dea_brands$class,
                                   dea_street_names$class,
                                   dea_class_cat$class)) %>%
  distinct() %>%
  arrange(allClass)

library(sqldf)

# lookup table
lookup_df <- sqldf("select cc.category, a.class, a.syn synonym from
                (select b.class, b.brand as syn from dea_brands as b
                 union
                 select s1.class, s1.slang as syn from dea_street_names as s1
                 union
                 select s2.class, s2.brand as syn from dea_street_names as s2 where s2.brand <> NULL) as a
              left join dea_class_cat as cc on a.class = cc.class")

use_data(lookup_df, overwrite = TRUE)



# summary of class info
x <- sqldf("select distinct c.allClass, b.Brand, s.Slang, cc.ClassCat from classes as c left join
           (select class, 'Yes' as Brand from dea_brands) as b on b.class = c.allClass
           left join
           (select class, 'Yes' as Slang from dea_street_names) as s on s.class = c.allClass
           left join
           (select class, 'Yes' as ClassCat from dea_class_cat) as cc on cc.class = c.allClass" )


# the controleld file has capitals mixed in
dea_controlled <- DOPE::dea_controlled %>%
  mutate(across(where(is.character), tolower))


# lookup table with substance (problematic because all versions of fentanyl)
synPlusSubstance <- sqldf(
  "select b.*, c.substance from
  (select cc.category, a.class, a.syn from
    (select b.class, b.brand as syn from dea_brands as b
      union
     select s1.class, s1.slang as syn from dea_street_names as s1
      union
     select s2.class, s2.brand as syn from dea_street_names as s2 where s2.brand <> NULL
    ) as a
  left join dea_class_cat as cc on a.class = cc.class) as b
left join dea_controlled as c on b.syn = c.synonym or b.class=c.synonym")


