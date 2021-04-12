###  Test with multiple category matches  ###
testOut_df <- data.frame(
  original_word = c("zip", "zip", "zip"),
  class = c("stimulant", "cannabis", "stimulant"),
  category = c("cocaine", "marijuana", "methamphetamine"),
  match = c("zip", "zip", "zip")
)

test_that("multiple category match works", {
  expect_equal(testOut_df, lookup_syn("zip"))
})

### Test for single category match ###
testOut_df2 <- data.frame(
  class = rep(c("cannabis"),each=5),
  category_match = rep(c("marijuana"), each= 5),
  synonym = c("420", "a-bomb", "acapulco gold", "acapulco red", "ace")
)

test_that("single category match works", {
  expect_equal(testOut_df2, head(lookup_syn("draf weed"),5))
})
