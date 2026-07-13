# ==============================================================================
# Tests: calculo general
# Tests: general calculation
# Archivo / File: tests/testthat/test-calculo.R
# ==============================================================================


make_calculation_data <- function() {
  
  set.seed(456)
  
  data.frame(
    dept = rep(c("A", "B", "C"), each = 40),
    strata = rep(c("S1", "S2", "S3"), each = 40),
    cluster = rep(seq_len(30), each = 4),
    weight = runif(120, 10, 50),
    ind_1 = rep(c(0, 1, 1, 0, 1), length.out = 120),
    ind_2 = rep(c(1, 0, 0, 1, 1, 0), length.out = 120),
    stringsAsFactors = FALSE
  )
}


make_calculation_cfg <- function(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    deff = TRUE,
    cv = TRUE
) {
  svySE_cfg(
    estimator = estimator,
    variance = "taylor",
    lonely_psu = "adjust",
    conf_level = 0.95,
    target = target,
    valid_values = valid_values,
    truncate_lower_ci = TRUE,
    pct_mult = 100,
    deff = deff,
    cv = cv,
    na_rm = TRUE
  )
}


test_that("svySE_calc returns a valid result with basic workflow", {
  
  skip_if_not_installed("survey")
  
  result <- svySE_calc(
    data = make_calculation_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_calculation_cfg(),
    verbose = FALSE
  )
  
  expect_s3_class(result, "svySE_result")
  expect_identical(names(result$results), "ind_1")
  expect_identical(names(result$results$ind_1), "error")
  expect_identical(names(result$results$ind_1$error), "TOTAL")
  expect_false("simple" %in% names(result$results$ind_1))
  
  expect_identical(result$meta$indicators, "ind_1")
  expect_identical(result$meta$group_vars, "dept")
  expect_identical(result$meta$group_labels, "Department")
  expect_identical(result$meta$strata, "strata")
  expect_identical(result$meta$cluster, "cluster")
  expect_identical(result$meta$weight, "weight")
  expect_null(result$meta$division)
  expect_null(result$meta$div_weight)
})


test_that("svySE_calc works with multiple indicators", {
  
  skip_if_not_installed("survey")
  
  result <- svySE_calc(
    data = make_calculation_data(),
    indicators = c("ind_1", "ind_2"),
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_calculation_cfg(),
    verbose = FALSE
  )
  
  expect_setequal(names(result$results), c("ind_1", "ind_2"))
  
  for (indicator in c("ind_1", "ind_2")) {
    expect_identical(names(result$results[[indicator]]), "error")
    expect_true("TOTAL" %in% names(result$results[[indicator]]$error))
    expect_false("simple" %in% names(result$results[[indicator]]))
  }
})


test_that("sampling error table contains the expected columns", {
  
  skip_if_not_installed("survey")
  
  result <- svySE_calc(
    data = make_calculation_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_calculation_cfg(),
    verbose = FALSE
  )
  
  tab <- result$results$ind_1$error$TOTAL
  
  expected <- c(
    "dept",
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
  
  expect_identical(names(tab), expected)
  expect_equal(tab$dept[1], "NACIONAL")
  expect_setequal(tab$dept[-1], c("A", "B", "C"))
})


test_that("national unweighted target count is correct", {
  
  skip_if_not_installed("survey")
  
  df <- make_calculation_data()
  
  result <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_calculation_cfg(),
    verbose = FALSE
  )
  
  tab <- result$results$ind_1$error$TOTAL
  expected_n <- sum(df$ind_1 == 1)
  
  expect_equal(tab$n_unw[1], expected_n)
  
  for (dept_name in c("A", "B", "C")) {
    expected_group_n <- sum(df$dept == dept_name & df$ind_1 == 1)
    observed_group_n <- tab$n_unw[tab$dept == dept_name]
    
    expect_equal(observed_group_n, expected_group_n)
  }
})


test_that("percentage estimates are expressed using pct_mult", {
  
  skip_if_not_installed("survey")
  
  df <- make_calculation_data()
  df$weight <- 1
  
  cfg_100 <- make_calculation_cfg()
  cfg_1 <- svySE_cfg(
    estimator = "prop",
    lonely_psu = "adjust",
    target = 1,
    valid_values = c(0, 1),
    pct_mult = 1,
    deff = TRUE,
    cv = TRUE
  )
  
  result_100 <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    weight = "weight",
    cfg = cfg_100,
    verbose = FALSE
  )
  
  result_1 <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    weight = "weight",
    cfg = cfg_1,
    verbose = FALSE
  )
  
  pct_100 <- result_100$results$ind_1$error$TOTAL$est_pct
  pct_1 <- result_1$results$ind_1$error$TOTAL$est_pct
  
  expect_equal(pct_100, pct_1 * 100, tolerance = 1e-8)
})


test_that("cv and deff can be disabled", {
  
  skip_if_not_installed("survey")
  
  result <- svySE_calc(
    data = make_calculation_data(),
    indicators = "ind_1",
    group_vars = "dept",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_calculation_cfg(deff = FALSE, cv = FALSE),
    verbose = FALSE
  )
  
  tab <- result$results$ind_1$error$TOTAL
  
  expect_true(all(is.na(tab$cv)))
  expect_true(all(is.na(tab$deff)))
})


test_that("lower confidence limits are truncated at zero when requested", {
  
  cfg <- make_calculation_cfg()
  
  metrics <- svySE_metrics(
    est_abs = 1,
    est_pct = 5,
    se_abs = 2,
    se_pct = 10,
    ci_l_abs = -3,
    ci_l_pct = -15,
    ci_u_abs = 5,
    ci_u_pct = 25,
    cv = 10,
    deff = 1,
    n_unw = 2,
    cfg = cfg
  )
  
  expect_equal(metrics$ci_l_abs, 0)
  expect_equal(metrics$ci_l_pct, 0)
})


test_that("lower confidence limits are preserved when truncation is disabled", {
  
  cfg <- svySE_cfg(
    estimator = "prop",
    truncate_lower_ci = FALSE
  )
  
  metrics <- svySE_metrics(
    est_abs = 1,
    est_pct = 5,
    se_abs = 2,
    se_pct = 10,
    ci_l_abs = -3,
    ci_l_pct = -15,
    ci_u_abs = 5,
    ci_u_pct = 25,
    cv = 10,
    deff = 1,
    n_unw = 2,
    cfg = cfg
  )
  
  expect_equal(metrics$ci_l_abs, -3)
  expect_equal(metrics$ci_l_pct, -15)
})


test_that("svySE_calc does not modify the original data frame", {
  
  skip_if_not_installed("survey")
  
  df <- make_calculation_data()
  original <- df
  
  invisible(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      strata = "strata",
      cluster = "cluster",
      weight = "weight",
      cfg = make_calculation_cfg(),
      verbose = FALSE
    )
  )
  
  expect_identical(df, original)
  expect_false(".__svySE_cat__" %in% names(df))
  expect_false(".__svySE_group_id__" %in% names(df))
})


test_that("print.svySE_result returns the object invisibly", {
  
  skip_if_not_installed("survey")
  
  result <- svySE_calc(
    data = make_calculation_data(),
    indicators = "ind_1",
    group_vars = "dept",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_calculation_cfg(),
    verbose = FALSE
  )
  
  output <- capture.output(
    returned <- print(result)
  )
  
  expect_identical(returned, result)
  
  # Validate relevant printed content without depending on one exact header.
  expect_true(any(grepl("svySE", output, fixed = TRUE)))
  expect_true(any(grepl("Indicators", output, fixed = TRUE)))
  expect_true(any(grepl("Groups", output, fixed = TRUE)))
  expect_true(any(grepl("Strata", output, fixed = TRUE)))
  expect_true(any(grepl("Cluster", output, fixed = TRUE)))
  expect_true(any(grepl("Weight", output, fixed = TRUE)))
})



test_that("verbose mode reports indicator and division progress", {
  
  skip_if_not_installed("survey")
  
  expect_message(
    svySE_calc(
      data = make_calculation_data(),
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = make_calculation_cfg(),
      verbose = TRUE
    ),
    "Procesando"
  )
})