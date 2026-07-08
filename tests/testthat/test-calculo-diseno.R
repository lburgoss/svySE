test_that("svySE_calc supports weight only design", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 40),
    weight = runif(80, 10, 50),
    ind_1 = sample(c(0, 1), 80, replace = TRUE)
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
    strata = NULL,
    cluster = NULL,
    weight = "weight",
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_true("ind_1" %in% names(res$results))
  expect_null(res$meta$strata)
  expect_null(res$meta$cluster)
})


test_that("svySE_calc supports stratified design without cluster", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 40),
    strata = rep(c("S1", "S2"), each = 40),
    weight = runif(80, 10, 50),
    ind_1 = sample(c(0, 1), 80, replace = TRUE)
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
    cluster = NULL,
    weight = "weight",
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_equal(res$meta$strata, "strata")
  expect_null(res$meta$cluster)
})


test_that("svySE_calc supports clustered design without strata", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 40),
    cluster = rep(1:20, each = 4),
    weight = runif(80, 10, 50),
    ind_1 = sample(c(0, 1), 80, replace = TRUE)
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
    strata = NULL,
    cluster = "cluster",
    weight = "weight",
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_null(res$meta$strata)
  expect_equal(res$meta$cluster, "cluster")
})


test_that("svySE_calc supports stratified clustered design", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 48),
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
    cfg = cfg,
    verbose = FALSE
  )
  
  expect_s3_class(res, "svySE_result")
  expect_equal(res$meta$strata, "strata")
  expect_equal(res$meta$cluster, "cluster")
})