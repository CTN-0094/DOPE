
# dplyr::filter(
#   lookup_df,
#   class == "zip" | category == "zip" | synonym == "zip"
# )

###  Basic Test  ###
testOut_df <- data.frame(
  class = c("stimulants", "stimulants"),
  category = c("cocaine", "methamphetamine")
)
testLookup_df <- lookup("cocaine", "methamphetamine")

test_that("basic lookup compression works", {
  expect_equal(testOut_df, compress_lookup(testLookup_df))
})

###  Test Second Compression  ###
testOut_df <- data.frame(
  class = "stimulants"
)

test_that("lookup compression for category works", {
  expect_equal(
    testOut_df,
    compress_lookup(testLookup_df, compressCategory = TRUE)
  )
})


###  Test Sorting  ###
rayExample <- lookup("cheese", "pizza", "with", "a", "soda")

ray_df <- data.frame(
  class = c(
    "stimulants", "heroin", "hallucinogen", "cannabis", "stimulants"
  ),
  category = c("cocaine", "heroin", "lsd", "marijuana", "methamphetamine")
)

test_that("compressing Ray's example works", {
  expect_equal(ray_df, compress_lookup(rayExample))
})


raySorted_df <- data.frame(
  class = c(
    "cannabis", "hallucinogen", "heroin", "stimulants", "stimulants"
  ),
  category = c("marijuana", "lsd", "heroin", "cocaine", "methamphetamine")
)

test_that("compressing Ray's example works", {
  expect_equal(raySorted_df, compress_lookup(rayExample, sortOutput = TRUE))
})
