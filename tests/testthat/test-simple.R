# ==============================================================================
# Tests: tablas simples
# Tests: simple indicator tables
# Archivo / File: tests/testthat/test-simple.R
# ==============================================================================


test_that("svySE_simple keeps the original unweighted behavior", {
  
  data <- data.frame(
    group = c("A", "A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0, 1),
    weight = c(10, 20, 30, 40, 50)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    output = "unweighted",
    verbose = FALSE
  )
  
  expect_s3_class(
    result,
    "svySE_simple_result"
  )
  
  tab <- result$results$indicator$simple$TOTAL
  
  expect_identical(
    names(tab),
    c(
      "group",
      "freq_0",
      "pct_0",
      "freq_1",
      "pct_1",
      "freq_total",
      "pct_total"
    )
  )
  
  national <- tab[tab$group == "NACIONAL", , drop = FALSE]
  group_a <- tab[tab$group == "A", , drop = FALSE]
  group_b <- tab[tab$group == "B", , drop = FALSE]
  
  expect_equal(national$freq_0, 2)
  expect_equal(national$freq_1, 3)
  expect_equal(national$freq_total, 5)
  expect_equal(national$pct_0, 40)
  expect_equal(national$pct_1, 60)
  expect_equal(national$pct_total, 100)
  
  expect_equal(group_a$freq_0, 1)
  expect_equal(group_a$freq_1, 2)
  expect_equal(group_a$freq_total, 3)
  expect_equal(group_a$pct_1, 200 / 3)
  
  expect_equal(group_b$freq_0, 1)
  expect_equal(group_b$freq_1, 1)
  expect_equal(group_b$freq_total, 2)
  expect_equal(group_b$pct_1, 50)
  
  expect_identical(result$meta$output, "unweighted")
  expect_null(result$meta$weight)
})


test_that("svySE_simple calculates weighted frequencies", {
  
  data <- data.frame(
    group = c("A", "A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0, 1),
    weight = c(10, 20, 30, 40, 50)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "weighted",
    verbose = FALSE
  )
  
  tab <- result$results$indicator$simple$TOTAL
  
  expect_identical(
    names(tab),
    c(
      "group",
      "exp_0",
      "exp_1",
      "exp_total"
    )
  )
  
  national <- tab[tab$group == "NACIONAL", , drop = FALSE]
  group_a <- tab[tab$group == "A", , drop = FALSE]
  group_b <- tab[tab$group == "B", , drop = FALSE]
  
  expect_equal(national$exp_0, 50)
  expect_equal(national$exp_1, 100)
  expect_equal(national$exp_total, 150)
  
  expect_equal(group_a$exp_0, 10)
  expect_equal(group_a$exp_1, 50)
  expect_equal(group_a$exp_total, 60)
  
  expect_equal(group_b$exp_0, 40)
  expect_equal(group_b$exp_1, 50)
  expect_equal(group_b$exp_total, 90)
  
  expect_identical(result$meta$output, "weighted")
  expect_identical(result$meta$weight, "weight")
})


test_that("svySE_simple can return weighted and unweighted frequencies together", {
  
  data <- data.frame(
    group = c("A", "A", "A", "B", "B"),
    indicator = c(0, 1, 1, 0, 1),
    weight = c(10, 20, 30, 40, 50)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "both",
    verbose = FALSE
  )
  
  tab <- result$results$indicator$simple$TOTAL
  
  expect_identical(
    names(tab),
    c(
      "group",
      "freq_0",
      "pct_0",
      "freq_1",
      "pct_1",
      "freq_total",
      "pct_total",
      "exp_0",
      "exp_1",
      "exp_total"
    )
  )
  
  national <- tab[tab$group == "NACIONAL", , drop = FALSE]
  
  expect_equal(national$freq_0, 2)
  expect_equal(national$freq_1, 3)
  expect_equal(national$freq_total, 5)
  
  expect_equal(national$exp_0, 50)
  expect_equal(national$exp_1, 100)
  expect_equal(national$exp_total, 150)
  
  expect_identical(result$meta$output, "both")
})


test_that("weighted output requires a weight variable", {
  
  data <- data.frame(
    group = c("A", "A"),
    indicator = c(0, 1)
  )
  
  expect_error(
    svySE_simple(
      data = data,
      indicators = "indicator",
      group_vars = "group",
      output = "weighted",
      verbose = FALSE
    ),
    "Peso requerido|Weight required"
  )
  
  expect_error(
    svySE_simple(
      data = data,
      indicators = "indicator",
      group_vars = "group",
      output = "both",
      verbose = FALSE
    ),
    "Peso requerido|Weight required"
  )
})


test_that("svySE_simple rejects invalid weight specifications", {
  
  data <- data.frame(
    group = c("A", "A"),
    indicator = c(0, 1),
    weight_a = c(1, 2),
    weight_b = c(3, 4)
  )
  
  expect_error(
    svySE_simple(
      data = data,
      indicators = "indicator",
      group_vars = "group",
      weight = c("weight_a", "weight_b"),
      output = "weighted",
      verbose = FALSE
    ),
    "Peso invalido|Invalid weight"
  )
  
  expect_error(
    svySE_simple(
      data = data,
      indicators = "indicator",
      group_vars = "group",
      weight = "missing_weight",
      output = "weighted",
      verbose = FALSE
    ),
    "Variables no encontradas|Variables not found"
  )
})


test_that("missing weights do not contribute to weighted frequencies", {
  
  data <- data.frame(
    group = c("A", "A", "A"),
    indicator = c(0, 1, 1),
    weight = c(10, NA, 30)
  )
  
  expect_warning(
    result <- svySE_simple(
      data = data,
      indicators = "indicator",
      group_vars = "group",
      weight = "weight",
      output = "both",
      verbose = FALSE
    ),
    "valores perdidos|missing"
  )
  
  tab <- result$results$indicator$simple$TOTAL
  national <- tab[tab$group == "NACIONAL", , drop = FALSE]
  
  # Unweighted counts retain all valid indicator records.
  expect_equal(national$freq_total, 3)
  expect_equal(national$freq_1, 2)
  
  # Weighted counts ignore only the record with missing weight.
  expect_equal(national$exp_0, 10)
  expect_equal(national$exp_1, 30)
  expect_equal(national$exp_total, 40)
})


test_that("na_rm controls missing indicator handling", {
  
  data <- data.frame(
    group = c("A", "A", "B"),
    indicator = c(1, NA, 0),
    weight = c(10, 20, 30)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "both",
    na_rm = TRUE,
    verbose = FALSE
  )
  
  tab <- result$results$indicator$simple$TOTAL
  national <- tab[tab$group == "NACIONAL", , drop = FALSE]
  
  expect_equal(national$freq_total, 2)
  expect_equal(national$exp_total, 40)
  
  expect_error(
    svySE_simple(
      data = data,
      indicators = "indicator",
      group_vars = "group",
      output = "unweighted",
      na_rm = FALSE,
      verbose = FALSE
    ),
    "valores perdidos|missing values"
  )
})


test_that("groups without valid indicator records are omitted", {
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    indicator = c(0, 1, NA, NA),
    weight = c(1, 2, 3, 4)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    weight = "weight",
    output = "both",
    na_rm = TRUE,
    verbose = FALSE
  )
  
  tab <- result$results$indicator$simple$TOTAL
  
  expect_true("A" %in% tab$group)
  expect_false("B" %in% tab$group)
})


test_that("simple tables work by division for all output modes", {
  
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    division = c("X", "Y", "X", "Y"),
    indicator = c(0, 1, 1, 0),
    weight = c(10, 20, 30, 40)
  )
  
  result <- svySE_simple(
    data = data,
    indicators = "indicator",
    group_vars = "group",
    division = "division",
    weight = "weight",
    output = "both",
    verbose = FALSE
  )
  
  divisions <- names(
    result$results$indicator$simple
  )
  
  expect_setequal(
    divisions,
    c("TOTAL", "X", "Y")
  )
  
  expect_true(
    all(
      c(
        "freq_0",
        "freq_1",
        "freq_total",
        "exp_0",
        "exp_1",
        "exp_total"
      ) %in%
        names(result$results$indicator$simple$X)
    )
  )
})
