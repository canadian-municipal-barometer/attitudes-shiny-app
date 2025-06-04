test_that("statement data colnames are correct", {
  # NOTE: I can't set this up like a proper package, because my deployment
  # (Posit Connect Cloud) seems to require different structure than
  # what a real package needs, so the working directory doesn't set properly
  # in `testthat`
  setwd("../../") # TODO: There must be a better way...
  print(getwd())
  colnames <- c("tags", "statement", "var_name")
  data_en <- readRDS("data/statements_en.rds")
  data_fr <- readRDS("data/statements_fr.rds")
  test_colnames_en <- data_en |> colnames()
  test_colnames_fr <- data_fr |> colnames()
  expect_equal(sort(test_colnames_en), sort(colnames))
  expect_equal(sort(test_colnames_fr), sort(colnames))
})
