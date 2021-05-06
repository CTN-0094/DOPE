# Make drug_stop_words vector
# Layla Bouzoubaa
# 03/2021
# UPDATED: 20210506

# This vector contains drug specific stop-words, designed to be used with parse()

drug_stop_words <- c(
  "a", "few", "mg", "pills", "pill","days", "off", "bunch", "street", "tab",
  "tabs", "detox", "rx", "not", "unsure", "unknown", "clinic", "bottle",
  "unknkwn", "type", "patch", "pm", "which", "injection", "er", "medication",
  "mgs", "illicit", "iv", "left", "patches", "visit", "hcl", "plus", "hd",
  "bit", "cit", "sulf", "tart", "c-ject", "es", "hp", "syringe", "contin",
  "intensol", "ject", "calcium"
)

usethis::use_data(drug_stop_words)
