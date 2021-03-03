
# dplyr::filter(
#   lookup_df,
#   category == "zip" | class == "zip" | synonym == "zip"
# )

###  Basic Test  ###
testOut_df <- data.frame(
  category = c("stimulants", "cannabis", "stimulants"),
  class = c("cocaine", "marijuana", "methamphetamine"),
  synonym = c("zip", "zip", "zip")
)

test_that("basic lookup works", {
  expect_equal(testOut_df, lookup("zip"))
})


###  String Case Test  ###
test_that("lookup ignores cases", {
  expect_equal(testOut_df, lookup("Zip"))
})


# dplyr::filter(
#   lookup_df,
#   category %in% c("zip", "shrooms") |
#     class %in% c("zip", "shrooms") |
#       synonym %in% c("zip", "shrooms")
# )

###  Test Dots  ###
testOut2_df <- data.frame(
  category = c(
    "stimulants", "cannabis", "stimulants",  "hallucinogen", "hallucinogen"
  ),
  class = c(
    "cocaine", "marijuana", "methamphetamine", "mushrooms", "psilocybin"
  ),
  synonym = c("zip", "zip", "zip", "shrooms", "shrooms")
)

test_that("lookup with dots works", {
  expect_equal(testOut2_df, lookup("zip", "shrooms"))
})

###  Test Vector input  ###
test_vector <- c("zip", "shrooms")

test_that("lookup with vector as first argument works", {
  expect_equal(testOut2_df, lookup(test_vector))
})

output_string <- "Using `drug_vec` argument with other words is not allowed. Please see the examples."
test_that("lookup vector and args outputs proper error message", {
  expect_error(lookup(testOut_vector, "smack"), output_string, ignore.case = TRUE)
})

###  Test return of original word  ###
testOut_df3 <- data.frame(
  category = c("stimulants", "cannabis", "stimulants"),
  class = c("cocaine", "marijuana", "methamphetamine"),
  synonym = c("zip", "zip", "zip"),
  original_word = c("zip")
)


test_that("the original lookup word is returned in the dataframe", {
  expect_equal(testOut_df3, lookup("zip"))
})
