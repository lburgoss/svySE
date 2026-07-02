test_that("error column presets work", {
  expect_true("est_pct" %in% svySE_cols_err("pct"))
  expect_true("cv" %in% svySE_cols_err("quality"))
  
  expect_error(
    svySE_cols_err("custom", cols = "bad_column")
  )
})

test_that("simple table column presets work", {
  expect_true("freq_1" %in% svySE_cols_tab("target"))
  expect_true("pct_total" %in% svySE_cols_tab("full"))
  
  expect_error(
    svySE_cols_tab("custom", cols = "bad_column")
  )
})