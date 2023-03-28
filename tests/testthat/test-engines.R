

base_path <- "c:\\packages\\fetch\\tests\\testthat\\data"

base_path <- "./data"

test_that("engines1: get_data_rds() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.rds")
  
  
  res <- get_data_rds(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_rds(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_rds(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})


test_that("engines2: get_data_rda() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.rda")
  
  
  res <- get_data_rda(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_rda(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_rda(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})



test_that("engines3: get_data_sas7bdat() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.sas7bdat")
  
  
  res <- get_data_sas7bdat(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_sas7bdat(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_sas7bdat(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})




test_that("engines4: get_data_csv() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.csv")
  
  
  res <- get_data_csv(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_csv(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_csv(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})




test_that("engines5: get_data_xls() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.xls")
  
  
  res <- get_data_xls(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_xls(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_xls(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})




test_that("engines6: get_data_xlsx() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.xlsx")
  
  
  res <- get_data_xlsx(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_xlsx(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_xlsx(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})




test_that("engines7: get_data_dbf() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.dbf")
  
  
  res <- get_data_dbf(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_dbf(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_dbf(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})



test_that("engines8: get_data_xpt() function works as expected.", {
  
  fp <- file.path(base_path, "demo_studya.xpt")
  
  
  res <- get_data_xpt(fp, "demo_studya")
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res) > 0, TRUE)
  
  res1 <- get_data_xpt(fp, "demo_studya", expression(sex == 'M'))
  
  res1
  
  expect_equal(is.null(res1), FALSE)
  expect_equal(nrow(res1) > 0, TRUE)
  expect_equal(nrow(res) > nrow(res1), TRUE)
  
  res2 <- get_data_xpt(fp, "demo_studya", top = 5)
  
  res2
  
  expect_equal(is.null(res2), FALSE)
  expect_equal(nrow(res2) > 0, TRUE)
  expect_equal(nrow(res2) == 5, TRUE)
  
  
})
