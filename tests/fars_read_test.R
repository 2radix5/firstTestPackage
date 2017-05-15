library(testthat)
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
test_that("file reading", {
  setwd("~/firstTestPackage/extdata")
  fname <- "accident_2013.csv"
  fars2013 <- fars_read(fname)
  expect_equal(fars2013, fars_read(fname))

  })
