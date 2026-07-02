test_that("svySE_calc works with a simple simulated survey", {
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
  
  expect_s3_class(res, "svySE_result")
  expect_true("ind_1" %in% names(res$results))
  expect_true("TOTAL" %in% names(res$results$ind_1$error))
  expect_true("est_pct" %in% names(res$results$ind_1$error$TOTAL))
})