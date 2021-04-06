###  Basic Test  ###
testIn_vec <- c("1 oxycodone 1morphine",
                 "Codeine",
                 "Dilaudid",
                " Dilaudid, Percocet, Norco,",
                 "Dilaudid, Vicodin",
                 "HYDROCODONE/OXYCODONE",
                 "Heroin, Oxycodone, Methadone.",
                 "Hydrocodone",
                 "Hydrocodone/ibuprofen 7.5/200",
                 "LORATAB, OXYCOCONE",
                 "LORTAB",
                 "Lortab",
                 "Lortab and Percocet",
                 "Lortab, Dilaudid",
                 "METHADONE, LORTAB",
                 "MORPHINE PERKS",
                 "Methadone, Lorecet, Lorotab",
                 "Methadone, Oxycodone",
                 "Morphine")

out_vec <- parse(testIn_vec)
test_that("parse works", {
  expect_equal(out_vec, parse(testIn_vec))
})
