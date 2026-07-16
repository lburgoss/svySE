# ==============================================================================
# Tests: exportacion de resultados
# Tests: result export
# Archivo / File: tests/testthat/test-exportar.R
# ==============================================================================


test_that("unweighted simple tables are exported automatically", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    output = "unweighted",
    verbose = FALSE
  )
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_silent(
    svySE_xlsx(
      x = result,
      file_tab = file,
      cols_tab = NULL
    )
  )
  
  expect_true(file.exists(file))
  
  sheets <- openxlsx::getSheetNames(file)
  expect_length(sheets, 1)
  
  exported <- openxlsx::read.xlsx(
    file,
    sheet = sheets[1],
    colNames = FALSE
  )
  
  expect_true(nrow(exported) > 0)
})


test_that("weighted simple tables are exported automatically", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0),
    weight = c(10, 20, 30, 40)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "weighted",
    verbose = FALSE
  )
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_silent(
    svySE_xlsx(
      x = result,
      file_tab = file,
      cols_tab = NULL
    )
  )
  
  expect_true(file.exists(file))
})


test_that("both weighted and unweighted counts can be exported", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0),
    weight = c(10, 20, 30, 40)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "both",
    verbose = FALSE
  )
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_silent(
    svySE_xlsx(
      x = result,
      file_tab = file,
      cols_tab = svySE_cols_tab("counts")
    )
  )
  
  expect_true(file.exists(file))
})


test_that("custom simple columns are exported in the requested order", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0),
    weight = c(10, 20, 30, 40)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "both",
    verbose = FALSE
  )
  
  selected <- svySE_cols_tab(
    type = "custom",
    cols = c(
      "freq_1",
      "exp_1",
      "freq_total",
      "exp_total"
    )
  )
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_silent(
    svySE_xlsx(
      x = result,
      file_tab = file,
      cols_tab = selected
    )
  )
  
  expect_true(file.exists(file))
})


test_that("export rejects columns not calculated in the simple result", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A"),
    indicator = c(0, 1)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    output = "unweighted",
    verbose = FALSE
  )
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_error(
    svySE_xlsx(
      x = result,
      file_tab = file,
      cols_tab = svySE_cols_tab("expanded")
    ),
    "Columnas no disponibles|Columns unavailable"
  )
})


test_that("legacy unweighted simple objects remain exportable", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A", "B"),
    indicator = c(0, 1, 1)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    output = "unweighted",
    verbose = FALSE
  )
  
  # Simulate an object generated before the output metadata was introduced.
  result$meta$output <- NULL
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_silent(
    svySE_xlsx(
      x = result,
      file_tab = file,
      cols_tab = NULL
    )
  )
  
  expect_true(file.exists(file))
})


test_that("multiple simple outputs can be exported to one workbook", {
  
  skip_if_not_installed("openxlsx")
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0),
    weight = c(10, 20, 30, 40)
  )
  
  unweighted <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    output = "unweighted",
    verbose = FALSE
  )
  
  weighted <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "weighted",
    verbose = FALSE
  )
  
  file <- tempfile(fileext = ".xlsx")
  
  expect_silent(
    svySE_xlsx(
      x = list(
        Unweighted = unweighted,
        Weighted = weighted
      ),
      file_tab = file,
      cols_tab = NULL
    )
  )
  
  expect_true(file.exists(file))
  expect_length(
    openxlsx::getSheetNames(file),
    2
  )
})
