
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
