test_that("svySE_calc returns a valid result with basic workflow", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B", "C"), each = 40),
    strata = rep(c("S1", "S2", "S3"), each = 40),
    cluster = rep(1:30, each = 4),
    weight = runif(120, 10, 50),
    ind_1 = sample(c(0, 1), 120, replace = TRUE)
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
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_true("ind_1" %in% names(res$results))
  
  expect_true("error" %in% names(res$results$ind_1))
  expect_true("simple" %in% names(res$results$ind_1))
  
  expect_true("TOTAL" %in% names(res$results$ind_1$error))
  expect_true("TOTAL" %in% names(res$results$ind_1$simple))
  
  expect_equal(res$meta$indicators, "ind_1")
  expect_equal(res$meta$group_vars, "dept")
  expect_equal(res$meta$group_labels, "Department")
  expect_equal(res$meta$strata, "strata")
  expect_equal(res$meta$cluster, "cluster")
  expect_equal(res$meta$weight, "weight")
})


test_that("svySE_calc works with multiple indicators", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 50),
    strata = rep(c("S1", "S2"), each = 50),
    cluster = rep(1:25, each = 4),
    weight = runif(100, 10, 50),
    ind_1 = sample(c(0, 1), 100, replace = TRUE),
    ind_2 = sample(c(0, 1), 100, replace = TRUE)
  )
  
  cfg <- svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    lonely_psu = "adjust"
  )
  
  res <- svySE_calc(
    data = df,
    indicators = c("ind_1", "ind_2"),
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_true(all(c("ind_1", "ind_2") %in% names(res$results)))
  expect_true("error" %in% names(res$results$ind_1))
  expect_true("simple" %in% names(res$results$ind_2))
})