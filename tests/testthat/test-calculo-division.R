test_that("svySE_calc works with division and cluster", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 48),
    area = rep(c("Urban", "Rural"), times = 48),
    strata = rep(rep(c("S1", "S2"), each = 24), 2),
    cluster = rep(1:24, each = 4),
    weight = runif(96, 10, 50),
    ind_1 = sample(c(0, 1), 96, replace = TRUE)
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
    cluster = "cluster",
    weight = "weight",
    division = "area",
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_true("TOTAL" %in% names(res$results$ind_1$error))
  expect_true("Urban" %in% names(res$results$ind_1$error))
  expect_true("Rural" %in% names(res$results$ind_1$error))
})