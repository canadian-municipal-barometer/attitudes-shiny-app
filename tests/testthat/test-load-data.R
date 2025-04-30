test_that("statement data colnames are correct", {
  print(getwd())
  colnames <- c("tags", "statement", "var_name")
  data <- load_statements(data_lang = "en")
  test_colnames <- data |> colnames()

  expect_equal(sort(test_colnames), sort(colnames))
})
