# ==============================================================================
# Tests: seleccion de columnas
# Tests: column selection
# Archivo / File: tests/testthat/test-columnas.R
# ==============================================================================


test_that("svySE_cols_err_all returns every sampling error column", {

  expected <- c(
    "est_abs",
    "est_pct",
    "se_abs",
    "se_pct",
    "ci_l_abs",
    "ci_l_pct",
    "ci_u_abs",
    "ci_u_pct",
    "cv",
    "deff",
    "n_unw"
  )

  expect_identical(svySE_cols_err_all(), expected)
})


test_that("svySE_cols_tab_all returns every simple table column", {

  expected <- c(
    "freq_0",
    "pct_0",
    "freq_1",
    "pct_1",
    "freq_total",
    "pct_total"
  )

  expect_identical(svySE_cols_tab_all(), expected)
})


test_that("svySE_cols_err returns the expected predefined profiles", {

  expect_identical(
    svySE_cols_err("full"),
    svySE_cols_err_all()
  )

  expect_identical(
    svySE_cols_err("pct"),
    c("est_pct", "se_pct", "ci_l_pct", "ci_u_pct", "cv")
  )

  expect_identical(
    svySE_cols_err("abs"),
    c("est_abs", "se_abs", "ci_l_abs", "ci_u_abs", "cv", "deff", "n_unw")
  )

  expect_identical(
    svySE_cols_err("basic"),
    c("est_abs", "est_pct", "se_abs", "se_pct", "cv")
  )

  expect_identical(
    svySE_cols_err("quality"),
    c("est_pct", "se_pct", "cv", "deff", "n_unw")
  )
})


test_that("svySE_cols_tab returns the expected predefined profiles", {

  expect_identical(
    svySE_cols_tab("full"),
    svySE_cols_tab_all()
  )

  expect_identical(
    svySE_cols_tab("target"),
    c("freq_1", "pct_1", "freq_total", "pct_total")
  )

  expect_identical(
    svySE_cols_tab("freq"),
    c("freq_0", "freq_1", "freq_total")
  )

  expect_identical(
    svySE_cols_tab("pct"),
    c("pct_0", "pct_1", "pct_total")
  )
})


test_that("custom column profiles preserve valid user selections", {

  err_cols <- c("est_pct", "cv", "n_unw")
  tab_cols <- c("freq_1", "pct_1")

  expect_identical(
    svySE_cols_err("custom", cols = err_cols),
    err_cols
  )

  expect_identical(
    svySE_cols_tab("custom", cols = tab_cols),
    tab_cols
  )
})


test_that("column selectors reject invalid types", {

  expect_error(
    svySE_cols_err("invalid"),
    "arg"
  )

  expect_error(
    svySE_cols_tab("invalid"),
    "arg"
  )
})


test_that("custom column selectors require non-empty character vectors", {

  expect_error(
    svySE_cols_err("custom", cols = NULL),
    "al menos una columna"
  )

  expect_error(
    svySE_cols_tab("custom", cols = character(0)),
    "al menos una columna"
  )

  expect_error(
    svySE_cols_err("custom", cols = 1),
    "vector de texto"
  )

  expect_error(
    svySE_cols_tab("custom", cols = TRUE),
    "vector de texto"
  )
})


test_that("custom column selectors reject unknown column names", {

  expect_error(
    svySE_cols_err("custom", cols = c("est_pct", "unknown")),
    "Columnas no validas"
  )

  expect_error(
    svySE_cols_tab("custom", cols = c("freq_1", "unknown")),
    "Columnas no validas"
  )
})
