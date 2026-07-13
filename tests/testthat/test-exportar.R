# ==============================================================================
# Tests: exportacion de resultados
# Tests: result export
# Archivo / File: tests/testthat/test-exportar.R
# ==============================================================================


make_export_data <- function() {

  set.seed(789)

  data.frame(
    dept = rep(c("A", "B"), each = 30),
    weight = runif(60, 10, 50),
    ind_1 = rep(c(0, 1), length.out = 60),
    ind_2 = rep(c(1, 0, 1, 0, 0), length.out = 60),
    stringsAsFactors = FALSE
  )
}


make_export_error_result <- function(indicators = "ind_1") {

  cfg <- svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    lonely_psu = "adjust"
  )

  svySE_calc(
    data = make_export_data(),
    indicators = indicators,
    group_vars = "dept",
    group_labels = "Department",
    strata = NULL,
    cluster = NULL,
    weight = "weight",
    cfg = cfg,
    verbose = FALSE
  )
}


make_export_simple_result <- function(indicators = "ind_1") {

  svySE_simple(
    data = make_export_data(),
    indicators = indicators,
    group_vars = "dept",
    group_labels = "Department",
    verbose = FALSE
  )
}


test_that("svySE_xlsx exports one sampling error result", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  file_err <- tempfile(fileext = ".xlsx")

  output <- svySE_xlsx(
    x = make_export_error_result(),
    file_err = file_err,
    file_tab = NULL,
    overwrite = TRUE
  )

  expect_true(file.exists(file_err))
  expect_identical(output$file_err, file_err)
  expect_null(output$file_tab)
  expect_identical(output$selected, "Result")
  expect_identical(output$error_results, "Result")
  expect_length(output$simple_results, 0)

  expect_identical(
    openxlsx::getSheetNames(file_err),
    "ind_1_Error"
  )
})


test_that("svySE_xlsx exports one simple result", {

  skip_if_not_installed("openxlsx")

  file_tab <- tempfile(fileext = ".xlsx")

  output <- svySE_xlsx(
    x = make_export_simple_result(),
    file_err = NULL,
    file_tab = file_tab,
    overwrite = TRUE
  )

  expect_true(file.exists(file_tab))
  expect_null(output$file_err)
  expect_identical(output$file_tab, file_tab)
  expect_identical(output$selected, "Result")
  expect_length(output$error_results, 0)
  expect_identical(output$simple_results, "Result")

  expect_identical(
    openxlsx::getSheetNames(file_tab),
    "ind_1_Simple"
  )
})


test_that("svySE_xlsx exports multiple indicators from individual results", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  file_err <- tempfile(fileext = ".xlsx")
  file_tab <- tempfile(fileext = ".xlsx")

  svySE_xlsx(
    x = make_export_error_result(c("ind_1", "ind_2")),
    file_err = file_err,
    file_tab = NULL,
    overwrite = TRUE
  )

  svySE_xlsx(
    x = make_export_simple_result(c("ind_1", "ind_2")),
    file_err = NULL,
    file_tab = file_tab,
    overwrite = TRUE
  )

  expect_setequal(
    openxlsx::getSheetNames(file_err),
    c("ind_1_Error", "ind_2_Error")
  )

  expect_setequal(
    openxlsx::getSheetNames(file_tab),
    c("ind_1_Simple", "ind_2_Simple")
  )
})


test_that("svySE_xlsx consolidates named error and simple results", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  results <- list(
    Housing = make_export_error_result(c("ind_1", "ind_2")),
    Health = make_export_error_result("ind_1"),
    Education = make_export_simple_result(c("ind_1", "ind_2"))
  )

  file_err <- tempfile(fileext = ".xlsx")
  file_tab <- tempfile(fileext = ".xlsx")

  output <- svySE_xlsx(
    x = results,
    file_err = file_err,
    file_tab = file_tab,
    overwrite = TRUE
  )

  expect_true(file.exists(file_err))
  expect_true(file.exists(file_tab))

  expect_identical(
    output$selected,
    c("Housing", "Health", "Education")
  )
  expect_identical(output$error_results, c("Housing", "Health"))
  expect_identical(output$simple_results, "Education")

  error_sheets <- openxlsx::getSheetNames(file_err)
  simple_sheets <- openxlsx::getSheetNames(file_tab)

  expect_length(error_sheets, 3)
  expect_length(simple_sheets, 2)

  expect_true("Housing_ind_1_Error" %in% error_sheets)
  expect_true("Housing_ind_2_Error" %in% error_sheets)
  expect_true("Health_ind_1_Error" %in% error_sheets)
  expect_true("Education_ind_1_Simple" %in% simple_sheets)
  expect_true("Education_ind_2_Simple" %in% simple_sheets)
})


test_that("svySE_xlsx exports only selected named results", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  results <- list(
    Housing = make_export_error_result("ind_1"),
    Health = make_export_error_result("ind_2"),
    Education = make_export_simple_result("ind_1")
  )

  file_err <- tempfile(fileext = ".xlsx")

  output <- svySE_xlsx(
    x = results,
    select = "Health",
    file_err = file_err,
    file_tab = NULL,
    overwrite = TRUE
  )

  expect_identical(output$selected, "Health")
  expect_identical(output$error_results, "Health")
  expect_length(output$simple_results, 0)

  expect_identical(
    openxlsx::getSheetNames(file_err),
    "Health_ind_2_Error"
  )
})


test_that("svySE_xlsx supports custom column selections", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  file_err <- tempfile(fileext = ".xlsx")
  file_tab <- tempfile(fileext = ".xlsx")

  svySE_xlsx(
    x = make_export_error_result(),
    file_err = file_err,
    file_tab = NULL,
    cols_err = c("est_pct", "cv", "n_unw"),
    overwrite = TRUE
  )

  svySE_xlsx(
    x = make_export_simple_result(),
    file_err = NULL,
    file_tab = file_tab,
    cols_tab = c("freq_1", "pct_1", "freq_total"),
    overwrite = TRUE
  )

  error_data <- openxlsx::read.xlsx(
    file_err,
    sheet = 1,
    colNames = FALSE,
    rows = 10:12
  )

  simple_data <- openxlsx::read.xlsx(
    file_tab,
    sheet = 1,
    colNames = FALSE,
    rows = 10:12
  )

  expect_equal(ncol(error_data), 4)
  expect_equal(ncol(simple_data), 4)
})


test_that("svySE_xlsx requires at least one output file", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  expect_error(
    svySE_xlsx(
      x = make_export_error_result(),
      file_err = NULL,
      file_tab = NULL
    ),
    class = "svySE_error"
  )
})


test_that("svySE_xlsx rejects incompatible output type requests", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  expect_error(
    svySE_xlsx(
      x = make_export_error_result(),
      file_err = NULL,
      file_tab = tempfile(fileext = ".xlsx")
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_xlsx(
      x = make_export_simple_result(),
      file_err = tempfile(fileext = ".xlsx"),
      file_tab = NULL
    ),
    class = "svySE_error"
  )
})


test_that("svySE_xlsx validates named result lists", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  unnamed <- list(
    make_export_error_result(),
    make_export_simple_result()
  )

  expect_error(
    svySE_xlsx(
      x = unnamed,
      file_err = tempfile(fileext = ".xlsx"),
      file_tab = tempfile(fileext = ".xlsx")
    ),
    class = "svySE_error"
  )

  duplicated <- list(
    Result = make_export_error_result(),
    Result = make_export_simple_result()
  )

  expect_error(
    svySE_xlsx(
      x = duplicated,
      file_err = tempfile(fileext = ".xlsx"),
      file_tab = tempfile(fileext = ".xlsx")
    ),
    class = "svySE_error"
  )

  incompatible <- list(
    Valid = make_export_error_result(),
    Invalid = data.frame(x = 1:3)
  )

  expect_error(
    svySE_xlsx(
      x = incompatible,
      file_err = tempfile(fileext = ".xlsx"),
      file_tab = NULL
    ),
    class = "svySE_error"
  )
})


test_that("svySE_xlsx validates select names", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  results <- list(
    Housing = make_export_error_result(),
    Education = make_export_simple_result()
  )

  expect_error(
    svySE_xlsx(
      x = results,
      select = "Missing",
      file_err = tempfile(fileext = ".xlsx"),
      file_tab = NULL
    ),
    class = "svySE_error"
  )
})


test_that("svySE_xlsx rejects identical output paths", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  results <- list(
    Errors = make_export_error_result(),
    Simple = make_export_simple_result()
  )

  same_file <- tempfile(fileext = ".xlsx")

  expect_error(
    svySE_xlsx(
      x = results,
      file_err = same_file,
      file_tab = same_file
    ),
    class = "svySE_error"
  )
})


test_that("svySE_xlsx validates start_row and logical arguments", {

  skip_if_not_installed("survey")
  skip_if_not_installed("openxlsx")

  result <- make_export_error_result()
  file_err <- tempfile(fileext = ".xlsx")

  expect_error(
    svySE_xlsx(
      x = result,
      file_err = file_err,
      start_row = 3
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_xlsx(
      x = result,
      file_err = file_err,
      start_row = 4.5
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_xlsx(
      x = result,
      file_err = file_err,
      overwrite = 1
    )
  )

  expect_error(
    svySE_xlsx(
      x = result,
      file_err = file_err,
      keep_na = NA
    )
  )
})


test_that("sheet-name helper creates valid and unique names", {

  long_name <- paste(rep("VeryLongResultName", 4), collapse = "_")

  first <- svySE_unique_sheet_name(long_name)
  second <- svySE_unique_sheet_name(long_name, used = first)

  expect_lte(nchar(first), 31)
  expect_lte(nchar(second), 31)
  expect_false(identical(first, second))
  expect_match(second, "_2$")
})


test_that("sheet-name helper removes invalid XLSX characters", {

  cleaned <- svySE_sheet_name("A/B:C*D?E[F]")

  expect_false(grepl("[/\\:*?\\[\\]]", cleaned))
  expect_lte(nchar(cleaned), 31)
})
