# ==============================================================================
# Tests: seleccion de columnas
# Tests: column selection
# Archivo / File: tests/testthat/test-columnas.R
# ==============================================================================


test_that("all simple table columns include weighted frequencies", {
  
  expect_identical(
    svySE_cols_tab_all(),
    c(
      "freq_0",
      "pct_0",
      "freq_1",
      "pct_1",
      "freq_total",
      "pct_total",
      "exp_0",
      "exp_1",
      "exp_total"
    )
  )
})


test_that("simple column profiles return the expected columns", {
  
  expect_identical(
    svySE_cols_tab("full"),
    svySE_cols_tab_all()
  )
  
  expect_identical(
    svySE_cols_tab("unweighted"),
    c(
      "freq_0",
      "pct_0",
      "freq_1",
      "pct_1",
      "freq_total",
      "pct_total"
    )
  )
  
  expect_identical(
    svySE_cols_tab("target"),
    c(
      "freq_1",
      "pct_1",
      "freq_total",
      "pct_total"
    )
  )
  
  expect_identical(
    svySE_cols_tab("freq"),
    c(
      "freq_0",
      "freq_1",
      "freq_total"
    )
  )
  
  expect_identical(
    svySE_cols_tab("pct"),
    c(
      "pct_0",
      "pct_1",
      "pct_total"
    )
  )
  
  expect_identical(
    svySE_cols_tab("expanded"),
    c(
      "exp_0",
      "exp_1",
      "exp_total"
    )
  )
  
  expect_identical(
    svySE_cols_tab("counts"),
    c(
      "freq_0",
      "freq_1",
      "freq_total",
      "exp_0",
      "exp_1",
      "exp_total"
    )
  )
})


test_that("custom simple columns preserve the requested order", {
  
  selected <- svySE_cols_tab(
    type = "custom",
    cols = c(
      "freq_1",
      "exp_1",
      "freq_total",
      "exp_total"
    )
  )
  
  expect_identical(
    selected,
    c(
      "freq_1",
      "exp_1",
      "freq_total",
      "exp_total"
    )
  )
})


test_that("invalid simple column profiles and columns fail clearly", {
  
  expect_error(
    svySE_cols_tab("unknown"),
    "arg"
  )
  
  expect_error(
    svySE_cols_tab(
      type = "custom",
      cols = NULL
    ),
    "al menos una columna|at least one column"
  )
  
  expect_error(
    svySE_cols_tab(
      type = "custom",
      cols = c("freq_1", "not_a_column")
    ),
    "Columnas no validas|Invalid columns"
  )
})


test_that("sampling error column helpers remain unchanged", {
  
  expect_identical(
    svySE_cols_err("full"),
    svySE_cols_err_all()
  )
  
  expect_true(
    all(
      c(
        "est_pct",
        "se_pct",
        "ci_l_pct",
        "ci_u_pct",
        "cv"
      ) %in%
        svySE_cols_err("pct")
    )
  )
})
