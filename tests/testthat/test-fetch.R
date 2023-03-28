


base_path <- "c:\\packages\\fetch\\tests\\testthat\\data"

base_path <- "./data"


test_that("fetch1: load_data() function works as expected with rds.", {
  
  
  res <- load_catalog(list(), base_path, engines$rds)
  
  
  res
  
  
  res1 <- load_data(res$demo_studya)
  
  expect_equal(nrow(res1), 10)

  
  res2 <- load_data(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- load_data(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- load_data(res$demo_studya,  filter = expression(treatment == "Active"),  
                    top = 4)
  
  expect_equal(nrow(res4), 4)
  
})


test_that("fetch2: fetch() function works as expected with rds.", {
  
  
  res <- catalog(base_path, engines$rds)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                    top = 4)
  
  expect_equal(nrow(res4), 4)
  
})


test_that("fetch3: fetch() function works as expected with rda.", {
  
  
  res <- catalog(base_path, engines$rda)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})


test_that("fetch4: fetch() function works as expected with rdata.", {
  
  
  res <- catalog(base_path, engines$rdata)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})


test_that("fetch5: fetch() function works as expected with sas7bdat.", {
  
  
  res <- catalog(base_path, engines$sas7bdat)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})



test_that("fetch6: fetch() function works as expected with csv", {
  
  
  res <- catalog(base_path, engines$csv)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})


test_that("fetch7: fetch() function works as expected with xls", {
  
  
  res <- catalog(base_path, engines$xls)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})


test_that("fetch8: fetch() function works as expected with xlsx", {
  
  
  res <- catalog(base_path, engines$xlsx)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})

test_that("fetch9: fetch() function works as expected with dbf", {
  
  
  res <- catalog(base_path, engines$dbf)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})

test_that("fetch10: fetch() function works as expected with xpt", {
  
  
  res <- catalog(base_path, engines$xpt)
  
  res
  res$demo_studya
  
  
  res1 <- fetch(res$demo_studya)
  
  expect_equal(nrow(res1), 10)
  
  
  res2 <- fetch(res$demo_studya, filter = expression(treatment == "Active"))
  
  expect_equal(nrow(res2), 6)
  
  
  res3 <- fetch(res$demo_studya, top = 4)
  
  expect_equal(nrow(res3), 4)
  
  
  res4 <- fetch(res$demo_studya,  filter = expression(treatment == "Active"),  
                top = 4)
  
  expect_equal(nrow(res4), 4)
  
})

