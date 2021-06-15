
# Test parse()
# Gabriel and Ray
# 2021-06-15


######  Basic Tests  ##########################################################
test_that("basic parse case sensitive", {
  expect_equal(
    "oxycodone", parse("Oxycodone")
  )
})

test_that("stop words removed 1", {
  expect_equal(
    c("lortab", "percocet"), parse("Lortab and Percocet")
  )
})

test_that("stop words removed 2", {
  expect_equal(
    "cocaine", parse("cocaine mg")
  )
})

test_that("basic parse trim whitespace", {
  expect_equal(
    "oxycodone", parse(" oxycodone ")
  )
})

test_that("basic multi parse", {
  expect_equal(
    c("dilaudid", "percocet", "norco"), parse(" Dilaudid, Percocet, Norco,")
  )
})

test_that("remove backslash", {
  expect_equal(
    c("hydrocodone", "oxycodone"), parse("HYDROCODONE/OXYCODONE")
  )
})

test_that("parse whitespace delim", {
  expect_equal(
    c("hydrocodone", "oxycodone"), parse("HYDROCODONE     OXYCODONE")
  )
})



######  Bug Fixes  ############################################################
test_that("remove numbers / punct", {
  expect_equal(
    c("oxycodone", "morphine"), parse("1 oxycodone 1morphine+")
  )
})

test_that("split words on special symbols", {
  expect_equal(
    c("morphine", "cocaine"), parse("morphine+cocaine")
  )
})

test_that("removed smart quotes", {
  expect_equal(
    "ambien", parse(" “a bunch” of Ambien ")
  )
})

test_that("Bup/Nx is excepted", {
  expect_equal(
    c("cocaine", "bup/nx"), parse("cocaine, Bup/Nx")
  )
})

sentence <- '
  I was at a pary and I started with some Percocets and Vicodin.
  I then had “a bunch” of Ambien and then some Barbiturate (no idea which).
  My buddy Keith took promethazine (25mg), clonidine (0.1mg), but he needs a Bup/Nx
'
test_that("parsing a complex paragraph", {
  expect_equal(
    c(
      "pary", "started", "percocets", "vicodin", "ambien", "barbiturate",
      "idea", "buddy", "keith", "promethazine", "clonidine", "bup/nx"
    ),
    parse(sentence)
  )
})



#
# ###  Basic Test  ###
# testIn_vec <- c("1 oxycodone 1morphine",
#                  "Codeine",
#                  "Dilaudid",
#                 " Dilaudid, Percocet, Norco,",
#                  "Dilaudid, Vicodin",
#                  "HYDROCODONE/OXYCODONE",
#                  "Heroin, Oxycodone, Methadone.",
#                  "Hydrocodone",
#                  "Hydrocodone/ibuprofen 7.5/200",
#                  "LORATAB, OXYCOCONE",
#                  "LORTAB",
#                  "Lortab",
#                  "Lortab and Percocet",
#                  "Lortab, Dilaudid",
#                  "METHADONE, LORTAB",
#                  "MORPHINE PERKS",
#                  "Methadone, Lorecet, Lorotab",
#                  "Methadone, Oxycodone",
#                  "Morphine")
#
# out_vec <- parse(testIn_vec)
# test_that("parse works", {
#   expect_equal(out_vec, parse(testIn_vec))
# })
