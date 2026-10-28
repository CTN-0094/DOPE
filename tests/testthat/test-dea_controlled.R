test_that("dea_controlled has the expected structure", {
  expect_s3_class(dea_controlled, "tbl_df")
  expect_named(
    dea_controlled,
    c("substance", "number", "schedule", "narcotic", "synonym")
  )
  expect_type(dea_controlled$number, "double")
  expect_true(all(dea_controlled$schedule %in% c("I", "II", "III", "IV", "V")))
  expect_true(all(dea_controlled$narcotic %in% c("Y", "N")))
})

test_that("dea_controlled synonyms are clean", {
  expect_false(anyNA(dea_controlled$synonym))
  expect_false(any(dea_controlled$synonym == ""))
  # no stray white space
  expect_false(any(grepl("^\\s|\\s$|\\s{2,}", dea_controlled$synonym)))
  # no duplicated substance/synonym pairs
  expect_equal(
    nrow(dea_controlled),
    nrow(dplyr::distinct(dea_controlled, substance, synonym))
  )
})

test_that("dea_controlled synonyms are names, not notes", {
  notes <- paste0(
    "(?i)precursor|constituent|positional isomer|listed as|",
    "has been sold as|various others|combination products|any substance"
  )
  expect_false(any(grepl(notes, dea_controlled$synonym, perl = TRUE)))
  # the PDF has a few characters that can not be decoded
  expect_false(any(grepl("�", dea_controlled$synonym)))
  # legal definitions (with commas) are not split into "synonyms"
  expect_false(any(grepl("(?i)cfr|u\\.s\\.c", dea_controlled$synonym, perl = TRUE)))
  expect_false(any(grepl("^[0-9]+$", dea_controlled$synonym)))
})

test_that("dea_controlled keeps records without a code number", {
  # schedule V preparations have no code number in the PDF
  expect_true(any(
    dea_controlled$substance == "Dihydrocodeine preparations 100mg/(100 ml or 100 gm)" &
      dea_controlled$synonym == "Cophene-S" &
      is.na(dea_controlled$number)
  ))
  # and are not merged into the record above them
  expect_false(any(grepl(" V Y$", dea_controlled$substance)))
})

test_that("dea_controlled handles a positional isomer note with no `)`", {
  expect_true(any(
    dea_controlled$synonym ==
      "1-(1,3-benzodioxol-4-yl)-2-(pyrrolidin-1-yl)pentan-1-one (2,3-methylenedioxy pyrovalerone)"
  ))
})

test_that("dea_controlled has known drug synonyms", {
  has_pair <- function(sub, syn) {
    any(dea_controlled$substance == sub & dea_controlled$synonym == syn)
  }
  expect_true(has_pair("Fentanyl", "Duragesic"))
  expect_true(has_pair("Oxycodone", "OxyContin"))
  expect_true(has_pair("Heroin", "Diacetylmorphine"))
  expect_true(has_pair("Zolpidem", "Ambien"))
  # synonyms separated by commas
  expect_true(has_pair("Diethyltryptamine", "N,N-diethyltryptamine"))
  # synonyms inside a "Positional Isomer:" note
  expect_true(has_pair(
    "Methylone (3,4-Methylenedioxy-N-methylcathinone)",
    "2,3-methylenedioxymethcathinone (2,3-MDMC, 1-(benzo[d][1,3]dioxol-4-yl)-2-(methylamino)propan-1-one)"
  ))
  # synonyms separated by "or"
  expect_true(any(dea_controlled$synonym == "2-fluorofentanyl"))
  # MDMA is not a synonym for 2C-B (the PDF says 2C-B "has been sold as" MDMA)
  expect_false(has_pair("4-Bromo-2,5-dimethoxyphenethylamine", "MDMA"))
})
