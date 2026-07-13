# ==============================================================================
# Tests: disenos muestrales de svySE_calc
# Tests: svySE_calc survey designs
# Archivo / File: tests/testthat/test-calculo-diseno.R
# ==============================================================================


make_design_data <- function() {

  set.seed(123)

  data.frame(
    dept = rep(c("A", "B", "C"), each = 40),
    strata = rep(c("S1", "S2", "S3"), each = 40),
    cluster = rep(seq_len(30), each = 4),
    weight = runif(120, 10, 50),
    ind_1 = rep(c(0, 1, 1, 0, 1), length.out = 120),
    stringsAsFactors = FALSE
  )
}


make_design_cfg <- function() {
  svySE_cfg(
    estimator = "prop",
    variance = "taylor",
    lonely_psu = "adjust",
    conf_level = 0.95,
    target = 1,
    valid_values = c(0, 1),
    truncate_lower_ci = TRUE,
    pct_mult = 100,
    deff = TRUE,
    cv = TRUE,
    na_rm = TRUE
  )
}


expected_error_columns <- c(
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


check_design_result <- function(result, expected_strata, expected_cluster) {

  expect_s3_class(result, "svySE_result")
  expect_true("ind_1" %in% names(result$results))
  expect_true("error" %in% names(result$results$ind_1))
  expect_false("simple" %in% names(result$results$ind_1))
  expect_true("TOTAL" %in% names(result$results$ind_1$error))

  tab <- result$results$ind_1$error$TOTAL

  expect_true(all(expected_error_columns %in% names(tab)))
  expect_equal(nrow(tab), 4)
  expect_equal(tab$dept[1], "NACIONAL")
  expect_setequal(tab$dept[-1], c("A", "B", "C"))
  expect_true(all(is.finite(tab$est_abs)))
  expect_true(all(is.finite(tab$est_pct)))
  expect_true(all(tab$est_pct >= 0 & tab$est_pct <= 100))
  expect_true(all(tab$n_unw >= 0))

  expect_identical(result$meta$strata, expected_strata)
  expect_identical(result$meta$cluster, expected_cluster)
  expect_identical(result$meta$weight, "weight")
}


test_that("svySE_calc supports a weight-only design", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_design_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = NULL,
    cluster = NULL,
    weight = "weight",
    cfg = make_design_cfg(),
    verbose = FALSE
  )

  check_design_result(
    result = result,
    expected_strata = NULL,
    expected_cluster = NULL
  )
})


test_that("svySE_calc supports a stratified design without clusters", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_design_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = NULL,
    weight = "weight",
    cfg = make_design_cfg(),
    verbose = FALSE
  )

  check_design_result(
    result = result,
    expected_strata = "strata",
    expected_cluster = NULL
  )
})


test_that("svySE_calc supports a clustered design without strata", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_design_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = NULL,
    cluster = "cluster",
    weight = "weight",
    cfg = make_design_cfg(),
    verbose = FALSE
  )

  check_design_result(
    result = result,
    expected_strata = NULL,
    expected_cluster = "cluster"
  )
})


test_that("svySE_calc supports a stratified and clustered design", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_design_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_design_cfg(),
    verbose = FALSE
  )

  check_design_result(
    result = result,
    expected_strata = "strata",
    expected_cluster = "cluster"
  )
})


test_that("svySE_calc preserves all grouping variables in the output", {

  skip_if_not_installed("survey")

  df <- make_design_data()
  df$area <- rep(c("Urban", "Rural"), length.out = nrow(df))

  result <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = c("dept", "area"),
    group_labels = c("Department", "Area"),
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_design_cfg(),
    verbose = FALSE
  )

  tab <- result$results$ind_1$error$TOTAL

  expect_true(all(c("dept", "area") %in% names(tab)))
  expect_equal(tab$dept[1], "NACIONAL")
  expect_equal(tab$area[1], "NACIONAL")
  expect_equal(result$meta$group_vars, c("dept", "area"))
  expect_equal(result$meta$group_labels, c("Department", "Area"))
})


test_that("svySE_calc stores the configuration and result metadata", {

  skip_if_not_installed("survey")

  cfg <- make_design_cfg()

  result <- svySE_calc(
    data = make_design_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = cfg,
    strict = FALSE,
    verbose = FALSE
  )

  expect_identical(result$meta$indicators, "ind_1")
  expect_identical(result$meta$group_vars, "dept")
  expect_identical(result$meta$group_labels, "Department")
  expect_identical(result$meta$strata, "strata")
  expect_identical(result$meta$cluster, "cluster")
  expect_identical(result$meta$weight, "weight")
  expect_null(result$meta$division)
  expect_null(result$meta$div_weight)
  expect_identical(result$meta$cfg, cfg)
  expect_false(result$meta$strict)
})


test_that("print.svySE_result returns the result invisibly", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_design_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = make_design_cfg(),
    verbose = FALSE
  )

  output <- capture.output(
    returned <- print(result)
  )

  expect_identical(returned, result)
  expect_true(any(grepl("svySE sampling error result", output, fixed = TRUE)))
  expect_true(any(grepl("Simple tab : No", output, fixed = TRUE)))
})
