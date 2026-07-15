# ==============================================================================
# Tests: tablas simples
# Tests: simple indicator tables
# Archivo / File: tests/testthat/test-simple.R
# ==============================================================================


make_simple_test_data <- function() {

  data.frame(
    dept = rep(c("A", "B", "C"), each = 8),
    area = rep(c("Urban", "Rural"), length.out = 24),
    ind_1 = c(
      1, 1, 0, 0, 1, 0, 1, 0,
      1, 0, 0, 0, 1, 0, 0, 0,
      1, 1, 1, 0, 1, 1, 0, 0
    ),
    ind_2 = rep(c(0, 1, 1), length.out = 24),
    stringsAsFactors = FALSE
  )
}


test_that("svySE_simple returns a valid simple result", {

  result <- svySE_simple(
    data = make_simple_test_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    verbose = FALSE
  )

  expect_s3_class(result, "svySE_simple_result")
  expect_identical(names(result$results), "ind_1")
  expect_identical(names(result$results$ind_1), "simple")
  expect_identical(names(result$results$ind_1$simple), "TOTAL")
  expect_false("error" %in% names(result$results$ind_1))

  expect_identical(result$meta$indicators, "ind_1")
  expect_identical(result$meta$group_vars, "dept")
  expect_identical(result$meta$group_labels, "Department")
  expect_null(result$meta$strata)
  expect_null(result$meta$cluster)
  expect_null(result$meta$weight)
  expect_true(result$meta$na_rm)
})


test_that("svySE_simple calculates exact frequencies and percentages", {

  df <- data.frame(
    dept = rep(c("A", "B"), each = 4),
    ind_1 = c(1, 1, 0, 0, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  result <- svySE_simple(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    target = 1,
    valid_values = c(0, 1),
    pct_mult = 100,
    na_rm = TRUE,
    verbose = FALSE
  )

  tab <- result$results$ind_1$simple$TOTAL

  national <- tab[tab$dept == "NACIONAL", , drop = FALSE]
  group_a <- tab[tab$dept == "A", , drop = FALSE]
  group_b <- tab[tab$dept == "B", , drop = FALSE]

  expect_equal(national$freq_0, 5)
  expect_equal(national$pct_0, 62.5)
  expect_equal(national$freq_1, 3)
  expect_equal(national$pct_1, 37.5)
  expect_equal(national$freq_total, 8)
  expect_equal(national$pct_total, 100)

  expect_equal(group_a$freq_1, 2)
  expect_equal(group_a$pct_1, 50)
  expect_equal(group_b$freq_1, 1)
  expect_equal(group_b$pct_1, 25)
})


test_that("svySE_simple works with multiple indicators", {

  result <- svySE_simple(
    data = make_simple_test_data(),
    indicators = c("ind_1", "ind_2"),
    group_vars = "dept",
    group_labels = "Department",
    verbose = FALSE
  )

  expect_setequal(names(result$results), c("ind_1", "ind_2"))

  for (indicator in c("ind_1", "ind_2")) {
    expect_identical(names(result$results[[indicator]]), "simple")
    expect_identical(names(result$results[[indicator]]$simple), "TOTAL")
    expect_false("error" %in% names(result$results[[indicator]]))
  }
})


test_that("svySE_simple supports division variables", {

  result <- svySE_simple(
    data = make_simple_test_data(),
    indicators = "ind_1",
    group_vars = "dept",
    group_labels = "Department",
    division = "area",
    verbose = FALSE
  )

  expect_identical(
    names(result$results$ind_1$simple),
    c("TOTAL", "Rural", "Urban")
  )

  for (division_name in names(result$results$ind_1$simple)) {
    tab <- result$results$ind_1$simple[[division_name]]

    expect_equal(tab$dept[1], "NACIONAL")
  }

  expect_identical(result$meta$division, "area")
})


test_that("svySE_simple respects target and percentage multiplier", {

  df <- data.frame(
    dept = rep("A", 4),
    ind_1 = c(1, 1, 0, 0),
    stringsAsFactors = FALSE
  )

  result <- svySE_simple(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    target = 0,
    valid_values = c(0, 1),
    pct_mult = 1,
    verbose = FALSE
  )

  tab <- result$results$ind_1$simple$TOTAL
  national <- tab[tab$dept == "NACIONAL", , drop = FALSE]

  expect_equal(national$freq_1, 2)
  expect_equal(national$pct_1, 0.5)
  expect_equal(national$freq_0, 2)
  expect_equal(national$pct_0, 0.5)
  expect_equal(national$pct_total, 1)
})


test_that("na_rm TRUE excludes missing values from the denominator", {

  df <- data.frame(
    dept = c("A", "A", "A", "B", "B"),
    ind_1 = c(1, 0, NA, 1, NA),
    stringsAsFactors = FALSE
  )

  result <- svySE_simple(
    data = df,
    indicators = "ind_1",
    group_vars = "dept",
    na_rm = TRUE,
    verbose = FALSE
  )

  tab <- result$results$ind_1$simple$TOTAL
  national <- tab[tab$dept == "NACIONAL", , drop = FALSE]

  expect_equal(national$freq_total, 3)
  expect_equal(national$freq_1, 2)
  expect_equal(national$freq_0, 1)
  expect_equal(national$pct_1, 200 / 3, tolerance = 1e-8)
  expect_equal(national$pct_0, 100 / 3, tolerance = 1e-8)
})


test_that("na_rm TRUE omits groups containing only missing indicator values", {

  df <- data.frame(
    department = c("A", "A", "A", "A", "B", "B"),
    district = c("D1", "D1", "D2", "D2", "D3", "D3"),
    ind_1 = c(1, 0, NA, NA, 1, 1),
    stringsAsFactors = FALSE
  )

  result <- svySE_simple(
    data = df,
    indicators = "ind_1",
    group_vars = c("department", "district"),
    group_labels = c("Department", "District"),
    na_rm = TRUE,
    verbose = FALSE
  )

  tab <- result$results$ind_1$simple$TOTAL

  expect_true("D1" %in% tab$district)
  expect_false("D2" %in% tab$district)
  expect_true("D3" %in% tab$district)

  national <- tab[tab$department == "NACIONAL", , drop = FALSE]
  expect_equal(national$freq_total, 4)
})


test_that("na_rm FALSE stops when missing indicator values are present", {

  df <- data.frame(
    dept = c("A", "A", "B", "B"),
    ind_1 = c(1, 0, NA, 1),
    stringsAsFactors = FALSE
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      na_rm = FALSE,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_simple excludes invalid values when strict is FALSE", {

  df <- data.frame(
    dept = c("A", "A", "B", "B", "B"),
    ind_1 = c(0, 1, 2, NA, 1),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      valid_values = c(0, 1),
      na_rm = TRUE,
      strict = FALSE,
      verbose = FALSE
    ),
    "fuera de `valid_values`"
  )

  tab <- result$results$ind_1$simple$TOTAL
  national <- tab[tab$dept == "NACIONAL", , drop = FALSE]

  expect_equal(national$freq_total, 3)
  expect_equal(national$freq_1, 2)
  expect_equal(national$freq_0, 1)
})


test_that("svySE_simple stops on invalid values when strict is TRUE", {

  df <- data.frame(
    dept = c("A", "A", "B"),
    ind_1 = c(0, 2, 1),
    stringsAsFactors = FALSE
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      valid_values = c(0, 1),
      strict = TRUE,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_simple does not modify the original data", {

  df <- make_simple_test_data()
  original <- df

  invisible(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      verbose = FALSE
    )
  )

  expect_identical(df, original)
  expect_false(".__svySE_group_id__" %in% names(df))
  expect_false(".__svySE_cat__" %in% names(df))
})


test_that("svySE_simple validates na_rm", {

  df <- make_simple_test_data()

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      na_rm = NA,
      verbose = FALSE
    )
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      na_rm = 1,
      verbose = FALSE
    )
  )
})


test_that("svySE_simple validates its main arguments", {

  df <- make_simple_test_data()

  expect_error(
    svySE_simple(
      data = df,
      indicators = "missing",
      group_vars = "dept",
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "missing",
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      group_labels = c("Department", "Extra"),
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      target = 2,
      valid_values = c(0, 1),
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_simple(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      pct_mult = 0,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("print.svySE_simple_result returns the object invisibly", {

  result <- svySE_simple(
    data = make_simple_test_data(),
    indicators = "ind_1",
    group_vars = "dept",
    verbose = FALSE
  )

  output <- capture.output(
    returned <- print(result)
  )

  expect_identical(returned, result)
  expect_true(any(grepl("svySE simple result", output, fixed = TRUE)))
  expect_true(any(grepl("Remove NA", output, fixed = TRUE)))
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
  expect_true(any(grepl("observed sample", output, fixed = TRUE)))
})


test_that("verbose mode reports simple-table progress", {

  expect_message(
    svySE_simple(
      data = make_simple_test_data(),
      indicators = "ind_1",
      group_vars = "dept",
      verbose = TRUE
    ),
    "Procesando tabla simple"
  )
})
