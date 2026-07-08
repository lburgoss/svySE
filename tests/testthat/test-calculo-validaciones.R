test_that("svySE_calc returns error when cluster variable does not exist", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 30),
    strata = rep(c("S1", "S2"), each = 30),
    weight = runif(60, 10, 50),
    ind_1 = sample(c(0, 1), 60, replace = TRUE)
  )
  
  cfg <- svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    lonely_psu = "adjust"
  )
  
  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      group_labels = "Department",
      strata = "strata",
      cluster = "cluster_missing",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc returns error when strata variable does not exist", {
  
  set.seed(123)
  
  df <- data.frame(
    dept = rep(c("A", "B"), each = 30),
    cluster = rep(1:15, each = 4),
    weight = runif(60, 10, 50),
    ind_1 = sample(c(0, 1), 60, replace = TRUE)
  )
  
  cfg <- svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    lonely_psu = "adjust"
  )
  
  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      group_labels = "Department",
      strata = "strata_missing",
      cluster = "cluster",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})