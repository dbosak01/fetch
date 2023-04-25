


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


test_that("fetch11: fetch() function works with import spec on dbf", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$dbf)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "numeric")
  expect_equal(d[d$Column == "dob", "Class"], "numeric")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})

test_that("fetch12: fetch() function works with import spec on rds", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$rds)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})


test_that("fetch13: fetch() function works with import spec on rda", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$rda)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})


test_that("fetch14: fetch() function works with import spec on rdata", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$rdata)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})


test_that("fetch15: fetch() function works with import spec on sas7bdat", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$sas7bdat)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "numeric")
  expect_equal(d[d$Column == "dob", "Class"], "numeric")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})


test_that("fetch16: fetch() function works with import spec on xpt", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$xpt)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})


test_that("fetch17: fetch() function works with import spec on xls", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$xls)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "POSIXct POSIXt")
  expect_equal(d[d$Column == "dob", "Class"], "POSIXct POSIXt")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})


test_that("fetch18: fetch() function works with import spec on xlsx", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$xlsx)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "POSIXct POSIXt")
  expect_equal(d[d$Column == "dob", "Class"], "POSIXct POSIXt")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})

test_that("fetch19: fetch() function works with import spec on xlsx", {
  
  
  spc <- import_spec(patient = "character",
                     visit = "integer",
                     screendate = "character",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$csv)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "numeric")
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  
  dt <- fetch(d, import_specs = spc)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
  
  dt <- fetch(d, import_specs = import_spec(trim_ws = FALSE))
  
  expect_equal(class(dt$patient), "numeric")
  
})

