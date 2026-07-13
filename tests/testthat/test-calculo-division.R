# ==============================================================================
# Tests: divisiones en svySE_calc
# Tests: divisions in svySE_calc
# Archivo / File: tests/testthat/test-calculo-division.R
# ==============================================================================


make_division_data <- function() {

  set.seed(321)

  n <- 144

  data.frame(
    dept = rep(c("A", "B", "C"), each = 48),
    strata = rep(c("S1", "S2", "S3"), each = 48),
    cluster = rep(seq_len(36), each = 4),
    area = rep(c("Urban", "Rural"), length.out = n),
    weight = runif(n, 10, 40),
    div_weight = runif(n, 20, 60),
    ind_1 = rep(c(0, 1, 1, 0, 1, 0), length.out = n),
    ind_2 = rep(c(1, 0, 1, 1, 0, 0), length.out = n),
    stringsAsFactors = FALSE
  )
}


make_division_cfg <- function() {
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


test_that("svySE_calc creates TOTAL and category results for division", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_division_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    div_weight = NULL,
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  divisions <- names(result$results$ind_1$error)

  expect_identical(divisions, c("TOTAL", "Rural", "Urban"))
  expect_true(all(vapply(
    result$results$ind_1$error,
    is.data.frame,
    logical(1)
  )))

  expect_identical(result$meta$division, "area")
  expect_null(result$meta$div_weight)
})


test_that("division results preserve national and grouped rows", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_division_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  for (division_name in names(result$results$ind_1$error)) {

    tab <- result$results$ind_1$error[[division_name]]

    expect_equal(nrow(tab), 4)
    expect_equal(tab$dept[1], "NACIONAL")
    expect_setequal(tab$dept[-1], c("A", "B", "C"))
    expect_true(all(tab$est_pct >= 0 & tab$est_pct <= 100))
    expect_true(all(tab$n_unw >= 0))
  }
})


test_that("svySE_calc uses division weight when supplied", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_division_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    div_weight = "div_weight",
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  expect_identical(result$meta$weight, "weight")
  expect_identical(result$meta$division, "area")
  expect_identical(result$meta$div_weight, "div_weight")

  expect_true("TOTAL" %in% names(result$results$ind_1$error))
  expect_true("Urban" %in% names(result$results$ind_1$error))
  expect_true("Rural" %in% names(result$results$ind_1$error))
})


test_that("division weight changes category estimates but not TOTAL estimate", {

  skip_if_not_installed("survey")

  df <- make_division_data()
  df$div_weight <- ifelse(df$ind_1 == 1, df$weight * 5, df$weight)

  result_primary <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    div_weight = NULL,
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  result_div_weight <- svySE_calc(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    div_weight = "div_weight",
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  total_primary <- result_primary$results$ind_1$error$TOTAL$est_pct
  total_div <- result_div_weight$results$ind_1$error$TOTAL$est_pct

  expect_equal(total_primary, total_div)

  urban_primary <- result_primary$results$ind_1$error$Urban$est_pct
  urban_div <- result_div_weight$results$ind_1$error$Urban$est_pct

  expect_false(isTRUE(all.equal(urban_primary, urban_div)))
})


test_that("svySE_calc supports multiple indicators with division", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_division_data(),
    indicators = c("ind_1", "ind_2"),
    group_vars = "dept",
    group_labels = "Department",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  expect_setequal(names(result$results), c("ind_1", "ind_2"))

  for (indicator in c("ind_1", "ind_2")) {
    expect_identical(
      names(result$results[[indicator]]$error),
      c("TOTAL", "Rural", "Urban")
    )
    expect_false("simple" %in% names(result$results[[indicator]]))
  }
})


test_that("missing division values remain in TOTAL but not as a category", {

  skip_if_not_installed("survey")

  df <- make_division_data()
  df$area[seq(1, nrow(df), by = 12)] <- NA_character_

  expect_warning(
    result <- svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      group_labels = "Department",
      strata = "strata",
      cluster = "cluster",
      weight = "weight",
      division = "area",
      cfg = make_division_cfg(),
      verbose = FALSE
    ),
    "valores perdidos"
  )

  expect_identical(
    names(result$results$ind_1$error),
    c("TOTAL", "Rural", "Urban")
  )

  expected_total_n <- sum(df$ind_1 == 1, na.rm = TRUE)
  observed_total_n <- result$results$ind_1$error$TOTAL$n_unw[1]

  expect_equal(observed_total_n, expected_total_n)
})


test_that("division results contain only error output", {

  skip_if_not_installed("survey")

  result <- svySE_calc(
    data = make_division_data(),
    indicators = "ind_1",
    group_vars = "dept",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    division = "area",
    cfg = make_division_cfg(),
    verbose = FALSE
  )

  expect_identical(names(result$results$ind_1), "error")
  expect_false("simple" %in% names(result$results$ind_1))
})
