# Process Drugs of Abuse Lexicon
# layla bouzoubaa
# March 16, 2025
library(conflicted)
suppressMessages(conflict_prefer("filter", "dplyr"))
library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(DOPE)
library(usethis)

# Load the existing lookup table to understand structure and for later merging
data("lookup_df")
# load new dat from gh
# url source: "https://github.com/kristycarp/gpt3-lexicon/blob/main/lexicon/drugs_of_abuse_lexicon.tsv"
gpt_lex <- read_tsv("./inst/extdata/drugs_of_abuse_lexicon.tsv")

parse_synonyms <- function(synonym_str) {
  # Handle NULL or NA
  if(is.null(synonym_str) || is.na(synonym_str) || synonym_str == "") {
    return(character(0))
  }

  # Remove the outer single quotes
  clean_str <- str_sub(synonym_str, 2, -2)

  # Split by commas within single quotes
  parts <- str_split(clean_str, "','")[[1]]

  # Return as character vector
  return(parts)
}

# Function to process the GPT lexicon and create extension_data
process_gpt_lexicon <- function(gpt_lex, lookup_df) {
  # Ensure column names are standardized
  if("index term" %in% colnames(gpt_lex)) {
    gpt_lex <- gpt_lex %>%
      rename(index_term = `index term`)
  }

  if("GPT-3 synonyms" %in% colnames(gpt_lex)) {
    gpt_lex <- gpt_lex %>%
      rename(gpt_synonyms = `GPT-3 synonyms`)
  }

  # Initialize extension_data
  extension_data <- tibble(
    class = character(),
    category = character(),
    synonym = character()
  )

  # Process each row in the lexicon
  for(i in 1:nrow(gpt_lex)) {
    # Extract the index term and synonyms
    index_term <- gpt_lex$index_term[i]
    index_term_lower <- tolower(index_term)

    # Parse the synonyms
    synonyms <- parse_synonyms(gpt_lex$gpt_synonyms[i])

    # Process each synonym and the index term itself
    all_terms <- c(index_term, synonyms)

    for(term in all_terms) {
      term_lower <- tolower(term)

      # Check if the term exists in lookup_df
      if(!term_lower %in% lookup_df$synonym) {
        # If not, add it to extension_data with the index term as category
        extension_data <- bind_rows(
          extension_data,
          tibble(
            class = "unknown",  # We'll classify this later
            category = index_term_lower,
            synonym = term_lower
          )
        )
      }
    }
  }

  # Remove any duplicates that might have been created
  extension_data <- extension_data %>%
    distinct()

  return(extension_data)
}


# Function to classify the extension data by assigning classes to categories
classify_extension_data <- function(extension_data) {
  # Get unique categories
  unique_categories <- unique(extension_data$category)
  cat("Total unique categories to classify:", length(unique_categories), "\n")

  # Create a mapping function based on patterns in the categories
  classify_category <- function(category) {
    category <- tolower(category)

    # Use regex patterns to classify
    if(str_detect(category, "(?=.*am$)|(?=.*trite$)|(gbl)|(?=.*qualone$)|(?=.*ital$)|(?=.*barbit)|(?=.*azepam$)|(?=.*azolam$)")) {
      return("depressant")
    } else if(str_detect(category, "crack|(?=.*cathin)|(?=.*date$)|(ritalin)|(?=.*phetamine$)")) {
      return("stimulant")
    } else if(str_detect(category, "(?=.*amt$)|(?=.*2cb$)|(?=.*mine$)|(dmt)|(?=.*room)|(pcp)|(peyote)|(?=.*cyclidine$)")) {
      return("hallucinogen")
    } else if(str_detect(category, "(?=.*deine$)|(?=.*one$)|(?=.*fentanil$)|(?=.*morph)|(?=.*fent)|(?=.*dilaud)|(?=.*codone$)")) {
      return("narcotic (opioid)")
    } else if(str_detect(category, "(?=.*marijuana)|(?=.*cannab)")) {
      return("cannabis")
    } else if(category == "dextromethorphan") {
      return("antitussive")
    } else if(str_detect(category, "(?=.*nitrous)|(?=.*oxide$)|(?=.*inhalant)")) {
      return("inhalant")
    } else {
      return("unknown")
    }
  }
  # Create a mapping dataframe
  category_mapping <- tibble(
    category = unique_categories,
    mapped_class = sapply(unique_categories, classify_category)
  )

  # Display classification results
  class_counts <- table(category_mapping$mapped_class)
  cat("\nClassification results for categories:\n")
  print(class_counts)

  # Apply the mapping to the extension data
  classified_extension_data <- extension_data %>%
    left_join(category_mapping, by = "category") %>%
    mutate(class = mapped_class) %>%
    select(-mapped_class)

  return(classified_extension_data)
}

# Main workflow function
process_gpt_lexicon_workflow <- function() {
  cat("Processing GPT-3 Lexicon...\n")

  # Step 1: Create extension data with terms not in lookup_df
  extension_data <- process_gpt_lexicon(gpt_lex, lookup_df)
  cat("Found", nrow(extension_data), "terms not in lookup_df\n")

  # Step 2: Classify the extension data
  classified_extension_data <- classify_extension_data(extension_data)

  # Step 3: Remove any remaining unknowns if desired
  # Uncomment this if you want to remove unclassified terms
  # classified_extension_data <- classified_extension_data %>%
  #   filter(class != "unknown")

  # Step 4: Combine with existing lookup_df
  updated_lookup <- bind_rows(lookup_df, classified_extension_data)

  cat("\nOriginal lookup_df entries:", nrow(lookup_df), "\n")
  cat("New entries added:", nrow(classified_extension_data), "\n")
  cat("Total entries in updated table:", nrow(updated_lookup), "\n")

  # Step 5: Save the updated lookup table
  lookup_df <- updated_lookup
  usethis::use_data(lookup_df, overwrite = TRUE)

  cat("\nUpdated lookup_df saved successfully.\n")

  # Return the classified extension data for inspection
  return(classified_extension_data)
}

# Run the workflow
classified_data <- process_gpt_lexicon_workflow()
write_csv(classified_data, "./inst/extdata/gpt_lexicon_extension_data.csv")
