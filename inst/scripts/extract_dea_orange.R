# Extract dea_controlled from the DEA Orange Book
# Layla Bouzoubaa
# 10/2026
# UPDATED: 20261028

# This script replaces the manual steps described in vignettes/controlled.Rmd
#   (UltraEdit + Excel + Difficult_Edited.csv). It reads the "Controlled
#   Substances - Alphabetical Order" PDF from the DEA Orange Book and makes
#   dea_controlled: one row per substance and synonym (other name).
# Run it from the package root. It does NOT rebuild lookup_df; see
#   vignettes/make_lookup_df.Rmd for that.


library(conflicted)
suppressMessages(conflict_prefer("filter", "dplyr"))
suppressPackageStartupMessages(library(dplyr))
library(pdftools)  # pdf_text()
library(purrr)     # imap(), map()
library(stringr)
library(tidyr)     # unnest()


##### Download the PDF ---------------------------------------------------------

orange_url <-
  "https://www.deadiversion.usdoj.gov/schedules/orangebook/c_cs_alpha.pdf"
orange_pdf <- "inst/extdata/c_cs_alpha.pdf"

# set to FALSE to rebuild from the copy of the PDF already in inst/extdata
download_pdf <- TRUE

if (download_pdf) {
  download.file(orange_url, orange_pdf, mode = "wb")
}

orange_text <- pdf_text(orange_pdf)

# the edition date is printed in the footer of every page (e.g., 21-Oct-26)
orange_date <- str_extract(orange_text[1], "\\d{2}-[A-Z][a-z]{2}-\\d{2}")
message("DEA Orange Book edition: ", orange_date)


##### Helper functions ---------------------------------------------------------

# Join the pieces of a name that wrapped onto several lines. A piece ending in
#   a hyphen (e.g., "indol-3-" + "yl)") is glued on without a space.
join_wrapped <- function(pieces) {
  pieces <- str_squish(str_remove_all(pieces, "\u200b"))  # zero-width spaces
  pieces <- pieces[pieces != ""]
  if (length(pieces) == 0) return(NA_character_)

  out <- pieces[1]
  for (piece in pieces[-1]) {
    out <- if (str_detect(out, "-$")) paste0(out, piece) else paste(out, piece)
  }
  out
}

# Split a string on a regex, but only where the match is not inside () or [].
#   Chemical names use commas and semicolons inside brackets, so a plain
#   str_split() would cut them apart.
split_top_level <- function(x, pattern) {
  chars <- str_split(x, "")[[1]]
  depth <- cumsum(chars %in% c("(", "[")) - cumsum(chars %in% c(")", "]"))
  # depth at the start of each character
  depth <- c(0, head(depth, -1))

  hits <- str_locate_all(x, pattern)[[1]]
  hits <- hits[depth[hits[, "start"]] <= 0, , drop = FALSE]
  if (nrow(hits) == 0) return(x)

  starts <- c(1, hits[, "end"] + 1)
  ends <- c(hits[, "start"] - 1, nchar(x))
  str_sub(x, starts, ends)
}

# Pull out the text of a "(Positional Isomer: ...)" note. Returns the string
#   without the note and the text inside the note. If the closing parenthesis
#   is missing (it is for one record) the note runs to the end of the string.
split_isomers <- function(x) {
  start <- str_locate(x, "(?i)\\(positional isomers?:")[1, ]
  if (is.na(start[1])) return(list(names = x, isomers = NA_character_))

  chars <- str_split(x, "")[[1]]
  depth <- cumsum(chars == "(") - cumsum(chars == ")")
  close <- which(depth[start[2]:length(chars)] < depth[start[1]])[1]
  if (is.na(close)) {
    return(list(
      names = str_sub(x, 1, start[1] - 1),
      isomers = str_sub(x, start[2] + 1)
    ))
  }
  close <- start[2] + close - 1

  list(
    names = str_c(str_sub(x, 1, start[1] - 1), str_sub(x, close + 1)),
    isomers = str_sub(x, start[2] + 1, close - 1)
  )
}

# Remove one set of (), [], or "" when they wrap the entire name
remove_wrapper <- function(x) {
  wrapped <- str_detect(x, '^(\\(.*\\)|\\[.*\\]|".*")$') &
    map_lgl(str_sub(x, 2, -2), function(inside) {
      chars <- str_split(inside, "")[[1]]
      depth <- cumsum(chars %in% c("(", "[")) - cumsum(chars %in% c(")", "]"))
      all(depth >= 0)
    })
  if_else(wrapped, str_sub(x, 2, -2), x)
}


##### Find the records ---------------------------------------------------------

# Each page has a header row (SUBSTANCE CSCN SCH NARC OTHER NAMES) and a footer
#   (Abbreviations...). Columns are spaced differently on each page, so the
#   start of the OTHER NAMES column is found on each page's header.
orange_lines <- imap(orange_text, function(page_text, page_no) {
  lines <- str_split(page_text, "\n")[[1]]
  header <- which(str_detect(lines, "^SUBSTANCE\\s+CSCN"))
  footer <- which(str_detect(lines, "^Abbreviations:"))
  stopifnot(length(header) == 1, length(footer) == 1)

  tibble(
    page = page_no,
    other_col = str_locate(lines[header], "OTHER NAMES")[1, "start"],
    line = lines[(header + 1):(footer - 1)]
  )
}) %>%
  bind_rows() %>%
  filter(str_trim(line) != "")

# A record starts on the line with the code number, schedule and narcotic
#   flag. Lines below it (until the next record) are the wrapped parts of its
#   substance name (left) and other names (right). Some schedule V
#   preparations (e.g., "Codeine preparations - 200 mg/(100 ml or 100 gm)")
#   have no code number.
record_regex <- paste0(
  "^(.*?)\\s{2,}(?:(\\d{4}|N/A)\\s+)?(I|II|III|IV|V)\\s+(Y|N)(?:\\s+(.*))?$"
)

orange_records <- orange_lines %>%
  mutate(
    record = str_match(line, record_regex),
    is_start = !is.na(record[, 1]),
    record_id = cumsum(is_start),
    # 3 characters of slack for small misalignments in the PDF text
    cut = other_col - 3,
    substance_piece = if_else(is_start, record[, 2], str_sub(line, 1, cut)),
    other_piece = if_else(
      is_start, coalesce(record[, 6], ""), str_sub(line, cut + 1)
    )
  )

stopifnot(orange_records$is_start[1])

orange_book <- orange_records %>%
  group_by(record_id) %>%
  summarise(
    substance   = join_wrapped(substance_piece),
    number      = as.numeric(na_if(first(record[, 3]), "N/A")),
    schedule    = first(record[, 4]),
    narcotic    = first(record[, 5]),
    other_names = join_wrapped(other_piece),
    .groups = "drop"
  ) %>%
  select(-record_id)


##### Split the other names into synonyms --------------------------------------

# Notes about a drug that are not names for it
notes_regex <- paste0(
  "(?i)precursor|^constituent|^in coca leaves|^various others|",
  "combination products|has been sold as"
)

# Legal definitions that fill the whole OTHER NAMES column. These are dropped
#   before splitting because they contain commas.
legal_regex <- "(?i)^any substance|^\\(i\\) naturally derived"

split_synonyms <- function(other_names) {
  if (is.na(other_names) || str_detect(other_names, legal_regex)) {
    return(character(0))
  }

  # the PDF has a few characters that cannot be decoded (shown as U+FFFD);
  #   dropping them gives the correct names (e.g., "2,3-methylenedioxy...")
  other_names <- str_remove_all(other_names, "\\s?\ufffd")

  parts <- split_isomers(other_names)

  # Names are separated by semicolons. A few records use commas instead, but
  #   only when they have no semicolons.
  name_sep <- if (str_detect(parts$names, ";")) ";" else ",(?=\\s)"
  synonyms <- split_top_level(parts$names, name_sep)

  # Positional isomers are separated by semicolons or commas; a comma is also
  #   used without a space before "N-" (e.g., "(NEB),N-ethylbuphedrone") but
  #   not in "N,N-dimethyl...".
  if (!is.na(parts$isomers)) {
    synonyms <- c(
      synonyms,
      split_top_level(parts$isomers, ";|,(?=\\s)|(?<!N),(?=N-)")
    )
  }

  synonyms <- synonyms %>%
    str_squish() %>%
    str_remove("(?i)^listed as\\s+") %>%
    str_remove("\\s+and others$") %>%
    discard(~ .x == "" | str_detect(.x, notes_regex))

  # "or" and "and" separate names (e.g., "ortho-Fluorofentanyl or
  #   2-fluorofentanyl"), except in product descriptions
  #   (e.g., "ASA or APAP w/codeine")
  synonyms <- map(synonyms, function(synonym) {
    if (str_detect(synonym, "\\swith\\s|w/")) return(synonym)
    split_top_level(synonym, "\\s(?:or|and)\\s")
  }) %>%
    unlist()

  synonyms %>%
    str_squish() %>%
    remove_wrapper() %>%
    str_squish() %>%
    discard(~ .x == "") %>%
    unique()
}

dea_controlled <- orange_book %>%
  mutate(synonym = map(other_names, split_synonyms)) %>%
  select(-other_names) %>%
  # substances without other names are dropped, as in the original version
  unnest(synonym)


##### Check the data -----------------------------------------------------------

# every line with a schedule and narcotic flag became a record
stopifnot(
  sum(str_detect(orange_lines$line, "\\s{2,}(I|II|III|IV|V)\\s+(Y|N)(\\s|$)")) ==
    nrow(orange_book)
)
stopifnot(!anyNA(dea_controlled$schedule), !anyNA(dea_controlled$narcotic))

message(
  nrow(orange_book), " substances; ",
  n_distinct(dea_controlled$substance), " with synonyms; ",
  nrow(dea_controlled), " substance/synonym rows"
)


##### Save ---------------------------------------------------------------------

# version = 2 keeps the package working with R < 3.5 like the other datasets
usethis::use_data(dea_controlled, overwrite = TRUE, version = 2)
