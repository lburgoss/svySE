test_that("svySE_xlsx creates Excel files", {
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B", "C"), each = 50),
    strata = rep(c("A", "B", "C"), each = 50),
    weight = runif(150, 10, 50),
    ind_1 = sample(c(0, 1), 150, replace = TRUE)
  )
  
  cfg <- svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    lonely_psu = "adjust"
  )
  
  res <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    weight = "weight",
    cfg = cfg,
    verbose = FALSE
  )
  
  file_err <- tempfile(fileext = ".xlsx")
  file_tab <- tempfile(fileext = ".xlsx")
  
  svySE_xlsx(
    x = res,
    file_err = file_err,
    file_tab = file_tab,
    cols_err = svySE_cols_err("pct"),
    cols_tab = svySE_cols_tab("full")
  )
  
  expect_true(file.exists(file_err))
  expect_true(file.exists(file_tab))
})