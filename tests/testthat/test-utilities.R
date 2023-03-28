



test_that("utilities1: get_dictionary() function works as expected.", {
  
  
  res <- get_dictionary(mtcars, "mtcars")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 11)
  
  
})
