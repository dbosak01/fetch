

base_path <- "c:\\packages\\fetch\\tests\\testthat\\data"

base_path <- "./data"


test_that("catalog1: load_catalog() function works as expected with rds.", {
  
  
  res <- load_catalog(list(), base_path, engines$rds)
  
  
  res
  
  expect_equal(length(res) >= 2, TRUE)
  
  
  
})


test_that("catalog2: catalog() function works as expected with filters", {
  
  
  res <- catalog(base_path, engines$rds, filter = expression(inv == 1000))
  
  res
  res$demo_studya
  
  res1 <- fetch(res$demo_studya)
  
  res1
  
  expect_equal(nrow(res1), 5)
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == 'Active'))
  
  res2
  
  expect_equal(nrow(res2), 3)
  

})

test_that("catalog3: catalog() function pattern parameter works as expected", {
  
  
  res <- catalog(base_path, engines$rds, pattern = "*a")
  
  
  expect_equal(length(res), 1)
  expect_equal(names(res), "demo_studya")
  
  res <- catalog(base_path, engines$rds, pattern = "*b")
  
  
  expect_equal(length(res), 1)
  expect_equal(names(res), "demo_studyb")
  
  
})
