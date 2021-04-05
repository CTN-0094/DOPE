###  Basic Test  ###
testOut_df <- data.frame(
  original_word = c("zip", "zip", "zip"),
  class = c("stimulants", "cannabis", "stimulants"),
  category = c("cocaine", "marijuana", "methamphetamine"),
  synonym = c("zip", "zip", "zip")
)

print(lookup("zip"))
test_that("basic lookup works", {
  expect_equal(testOut_df, lookup("zip"))
})


###  String Case Test  ###
test_that("lookup ignores cases", {
  expect_equal(testOut_df, lookup("Zip"))
})


###  Test Dots  ###
testOut2_df <- data.frame(
  original_word = c("zip", "zip", "zip", "shrooms", "shrooms"),
  class = c(
    "stimulants", "cannabis", "stimulants",  "hallucinogen", "hallucinogen"
  ),
  category = c(
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

###  Test return of original word  ###
testOut_df3 <- data.frame(
  original_word = c("zip"),
  class = c("stimulants", "cannabis", "stimulants"),
  category = c("cocaine", "marijuana", "methamphetamine"),
  synonym = c("zip", "zip", "zip")
)


test_that("the original lookup word is returned in the dataframe", {
  expect_equal(testOut_df3, lookup("zip"))
})
