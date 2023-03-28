

base_path <- "c:\\packages\\fetch\\tests\\testthat\\data"

base_path <- "./data"


test_that("catalog1: load_catalog() function works as expected with rds.", {
  
  
  res <- load_catalog(list(), base_path, engines$rds)
  
  
  res
  
  expect_equal(length(res) >= 2, TRUE)
  
  
  
})
