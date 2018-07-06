context("Get traits from FieldbookApp-SPBase files")

test_that("get traits ", {
  
  library(traittools)
  library(readr)
  fb <- read_csv(test_sheet("fbapp_tableFormat_fumesua.csv"))
  names(fb) <- gsub("[[:space:]]", "", names(fb)) #remove whitespaces
  
  traits<- get_trait_fb(fieldbook = fb, dsource = 2)
  
  expected_traits <- c("ALCDAM|CO_331:0000806",  "DMFV|CO_331:0000251", 
                       "MILLDAM|CO_331:0000805", "NOCR|CO_331:0000214",  
                       "NONC|CO_331:0000217",    "NOPE|CO_331:0000192",   
                       "NOPH|CO_331:0000679" ,   "NOPS|CO_331:0000678",    "NOPR|CO_331:0000211",   
                       "RTFSH1|CO_331:0000178",  "RTSKN1|CO_331:0000175",  "WED1|CO_331:0000207",   
                       "RTDEF|CO_331:0000790" ,  "RTSHP|CO_331:0000181",   "DAMR|CO_331:0000206",   
                       "RF|CO_331:0000202",      "RS|CO_331:0000184",      "VV1|CO_331:0000197" ,   
                       "VIR1|CO_331:0000193",    "VIR2|CO_331:2000004",    "CRW|CO_331:0000220" ,   
                       "NCRW|CO_331:0000223",    "VW|CO_331:0000227")

  expect_identical(traits, expected_traits)
  
})