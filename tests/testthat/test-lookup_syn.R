### Test for single category match ###
testOut_df <- data.frame(
  class = rep(c("cannabis"),each=5),
  category_match = rep(c("marijuana"), each= 5),
  synonym = c("420", "a-bomb", "acapulco gold", "acapulco red", "ace")
)

test_that("single category match works", {
  expect_equal(testOut_df, head(lookup_syn("draf weed"),5))
})

###  Test with multiple category matches  ###
testOut_df2 <- data.frame(
  category =c(
    "amphetamine"
    ,"cocaine"
    ,"crack"
    ,"crack cocaine"
    ,"khat"
    ,"methamphetamine"
    ,"methcathinone"
    ,"methylphenidate"
    ,"ritalin"
    ,"synthetic cathinone"
  )
)

test_that("multiple category match works", {
  expect_equal(testOut_df2$category, lookup_syn("stimulant")$category)
})
