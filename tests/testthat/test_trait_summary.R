context("Test for summary test")

test_that("trait_summary test", {

  library(traittools)
  library(st4gi)
  
  sm1 <- trait_summary(fieldbook = spg, trait = "dm", genotype = "geno",factor = "loc",trait_type = "numerical")
  testthat::expect_equal(nrow(sm1),16)
  
  sm2 <- trait_summary(fieldbook = spg, trait = "dm", genotype = "geno",factor = c("loc","rep"),trait_type = "numerical")
  testthat::expect_equal(nrow(sm2),32)
  
})  

test_that("wrong trait_type", {
  
  library(traittools)
  #sm <- trait_summary(fieldbook = spg, trait = "dm", genotype = "geno",factor = c("loc","rep"),trait_type = "numericales")
  testthat::expect_message(trait_summary(fieldbook = spg, trait = "dm", genotype = "geno",factor = c("loc","rep"),trait_type = "numericales") , 
                    "Write correctly the type of trait. It must be 'numerical' or 'categorical'")
  
})  


test_that("sub sample data", {
  
  library(traittools)
  fb <- subsample
  fb$SSAMPLE <- as.factor(fb$SSAMPLE)
  as <- trait_summary(fieldbook =fb, trait = "yield", genotype = "INSTN", factor = c("SSAMPLE"),trait_type = "numerical")
  testthat::expect_equal(nrow(as),100)
})

test_that("factor with sub sample data", {
  
  library(traittools)
  fb <- fctsubsample
  #fb$SSAMPLE <- as.factor(fb$SSAMPLE)
  as <- trait_summary(fieldbook = fb, trait = "yield", genotype = "INSTN", factor = c("FACTOR","SSAMPLE"),trait_type = "numerical")
  testthat::expect_equal(nrow(as),10)
})


test_that("CRD without factors", {
  
  library(traittools)
  library(fbdesign)
  fb <- fctsubsample
  
  #fb$SSAMPLE <- as.factor(fb$SSAMPLE)
  as <- summary_by_design(fieldbook = fb, trait = "yield", design = "CRD", genotype = "INSTN", trait_dict = table_module_potato)
  testthat::expect_equal(nrow(as),5)
})


test_that("CRD with factors", {
  
  library(traittools)
  library(fbdesign)
  fb <- fctsubsample
  
  #fb$SSAMPLE <- as.factor(fb$SSAMPLE)
  as <- summary_by_design(fieldbook = fb, trait = "yield", design = "CRD", genotype = "INSTN", factor = c("FACTOR"),trait_dict = table_module_potato)
  testthat::expect_equal(nrow(as),10)
})


test_that("RCBD without factors", {
  
  library(traittools)
  library(fbdesign)
  fb <- fctsubsample
  
  #fb$SSAMPLE <- as.factor(fb$SSAMPLE)
  as <- summary_by_design(fieldbook = fb, trait = "yield", design = "RCBD", genotype = "INSTN", trait_dict = table_module_potato)
  testthat::expect_equal(nrow(as),5)
})


test_that("Randomized Complete Block Design with factors", {
  
  library(traittools)
  library(fbdesign)
  fb <- fctsubsample
  as <- summary_by_design(fieldbook = fb, trait = "yield", design = "RCBD", genotype = "INSTN", factor = c("FACTOR"),trait_dict = table_module_potato)
  testthat::expect_equal(nrow(as),10)
})

test_that("Augmented block design with factors and subsample", {
  
  library(traittools)
  library(fbdesign)
  
  datos <- augbd
  hot_design <- "ABD"
  trait_dict <- table_module_potato
  trait <- traittools::get_trait_fb(datos)
  genofilter <- "INSTN"
  factor <- "FACTOR" 
  sumyfct <- traittools::trait_summary_join(fieldbook = datos, genotype = genofilter,
                                       factor = factor, trait = trait,
                                       design = hot_design,trait_dict = trait_dict)
  
  testthat::expect_equal(nrow(sumyfct),807)
})



test_that("trait summary join with multilpe traits", {
  
  library(traittools)
  library(fbdesign)
  fb <- fctsubsample
  ds <- trait_summary_join(fieldbook = fb, genotype = "INSTN",  trait = "yield", 
                           design = "RCBD", trait_dict = table_module_potato ) 

})




