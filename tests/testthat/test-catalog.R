

base_path <- "c:\\packages\\fetch\\tests\\testthat\\data"

base_path <- "./data"


test_that("catalog1: load_catalog() function works as expected with rds.", {
  
  
  res <- load_catalog(list(), base_path, engines$rds)
  
  
  res
  
  expect_equal(length(res) >= 2, TRUE)
  
  
  
})


test_that("catalog2: catalog() function works as expected with where expression", {
  
  
  res <- catalog(base_path, engines$rds, where = expression(inv == 1000))
  
  res
  res$demo_studya
  
  res1 <- fetch(res$demo_studya)
  
  res1
  
  expect_equal(nrow(res1), 5)
  
  res2 <- fetch(res$demo_studya, where = expression(treatment == 'Active'))
  
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


test_that("catalog3: catalog() function works with import spec on rds", {
  
  
  
  spc <- import_spec(patient = "integer",
                     visit = "integer",
                     screendate = "date=%Y-%m-%d",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$rds)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  
  
  res <- catalog(base_path, engines$rds, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")


  dt <- fetch(d)
  
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "Date")
  
  
})

test_that("catalog4: catalog() function works with import spec on rda", {
  
  
  
  spc <- import_spec(patient = "integer",
                     visit = "integer",
                     screendate = "date=%Y-%m-%d",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$rda)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  
  
  res <- catalog(base_path, engines$rda, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "Date")
  

})

test_that("catalog5: catalog() function works with import spec on rdata", {
  

  spc <- import_spec(patient = "integer",
                     visit = "integer",
                     screendate = "date=%Y-%m-%d",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$rdata)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  
  
  res <- catalog(base_path, engines$rdata, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "Date")
  
})

test_that("catalog6: catalog() function works with import spec on sas7bdat", {
  
  
  spc <- import_spec(patient = "integer",
                     visit = "integer",
                     screendate = "date=%Y-%m-%d",
                     dob = "date=%Y-%m-%d")
  
  res <- catalog(base_path, engines$sas7bdat)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "numeric")
  expect_equal(d[d$Column == "screendate", "Class"], "numeric")
  
  
  res <- catalog(base_path, engines$sas7bdat, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "Date")
})

test_that("catalog7: catalog() function works with import spec on xls", {
  
  
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
  
  
  res <- catalog(base_path, engines$xls, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "character")
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "character")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  dt <- fetch(d)

  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
})


test_that("catalog8: catalog() function works with import spec on xlsx", {
  
  
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
  
  
  res <- catalog(base_path, engines$xlsx, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "character")
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "character")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
})


test_that("catalog9: catalog() function works with import spec on xpt", {
  
  
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
  
  
  res <- catalog(base_path, engines$xpt, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "character")
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "character")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
})

test_that("catalog10: catalog() function works with import spec on dbf", {
  
  
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
  
  
  res <- catalog(base_path, engines$dbf, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "character")
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "character")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
})

test_that("catalog11: catalog() function works with import spec on csv", {
  
  
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
  
  
  res <- catalog(base_path, engines$dbf, import_specs = spc)
  
  d <- res$demo_studya
  
  expect_equal(d[d$Column == "patient", "Class"], "character")
  expect_equal(d[d$Column == "visit", "Class"], "integer")
  expect_equal(d[d$Column == "screendate", "Class"], "character")
  expect_equal(d[d$Column == "dob", "Class"], "Date")
  
  dt <- fetch(d)
  
  expect_equal(class(dt$patient), "character")
  expect_equal(class(dt$visit), "integer")
  expect_equal(class(dt$screendate), "character")
  expect_equal(class(dt$dob), "Date")
  
})

