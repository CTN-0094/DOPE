### Test for single category match ###
testOut_df <- data.frame(
  class = rep("cannabis", each = 5),
  category_match = rep("marijuana", each = 5),
  synonym = c("420", "a-bomb", "acapulco gold", "acapulco red", "ace")
)

test_that("single category match works", {
  expect_equal(
    testOut_df,
    head(lookup_syn("draf weed"), 5)
  )
})


###  Test that Multiple Entries are Discarded  ###
test_that("Drug vector only uses first entry and throws warning", {
  expect_warning(
    lookup_syn(c("ritalin", "opium")),
    "Only the first element"
  )
})


###  Test Drug Category Search  ###
# This is the else if() part
test_that("Drug category matches multiple times handled correctly", {
  expect_equal(
    data.frame(
      class = "stimulant",
      category_match = "ritalin",
      synonym = "kibbles and bits"
    ),
    lookup_syn("ritalin")
  )
})


###  Test with multiple category matches  ###
testOut_df2 <- data.frame(
  category = c(
    "amphetamine", "cocaine", "crack", "crack cocaine", "khat",
    "methamphetamine", "methcathinone", "methylphenidate", "ritalin",
    "synthetic cathinone"
  )
)

test_that("multiple category match works", {
  expect_equal(
    testOut_df2$category,
    lookup_syn("stimulant")$category
  )
})
