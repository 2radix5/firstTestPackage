library(testthat)
test_that("file reading", {
  fname <- "../extdata/accident_2013.csv"
  fars2013 <- fars_read(fname)
  expect_equal(fars2013, fars_read(fname))

  })
