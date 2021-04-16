###  Basic Test  ###
testOut_df <- data.frame(
  original_word = c("cocaine", "methamphetamine"),
  class = c("stimulant", "stimulant"),
  category = c("cocaine", "methamphetamine")
)
testLookup_df <- lookup("cocaine", "methamphetamine")

test_that("basic lookup compression works", {
  expect_equal(testOut_df, compress_lookup(testLookup_df))
})

###  Test Second Compression  ###
testOut_df <- data.frame(
  class = "stimulant"
)

test_that("lookup compression for category works", {
  expect_equal(
    testOut_df,
    compress_lookup(testLookup_df, compressOriginalWord = TRUE, compressCategory = TRUE)
  )
})
