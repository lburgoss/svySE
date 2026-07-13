# ==============================================================================
# Tests: validaciones de svySE_calc
# Tests: svySE_calc validations
# Archivo / File: tests/testthat/test-calculo-validaciones.R
# ==============================================================================


make_validation_data <- function() {

  data.frame(
    dept = rep(c("A", "B"), each = 12),
    strata = rep(c("S1", "S2"), each = 12),
    cluster = rep(seq_len(8), each = 3),
    weight = rep(10, 24),
    div_weight = rep(12, 24),
    area = rep(c("Urban", "Rural"), length.out = 24),
    ind_1 = rep(c(0, 1), length.out = 24),
    stringsAsFactors = FALSE
  )
}


make_validation_cfg <- function() {
  svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1),
    lonely_psu = "adjust"
  )
}


test_that("svySE_calc rejects invalid configuration objects", {

  skip_if_not_installed("survey")

  df <- make_validation_data()

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = list(estimator = "prop"),
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc rejects the ratio estimator", {

  skip_if_not_installed("survey")

  df <- make_validation_data()
  cfg_ratio <- svySE_cfg(estimator = "ratio")

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg_ratio,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc validates data and character arguments", {

  skip_if_not_installed("survey")

  df <- make_validation_data()
  cfg <- make_validation_cfg()

  expect_error(
    svySE_calc(
      data = matrix(1:4, nrow = 2),
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = 1,
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = 1,
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = NULL,
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc rejects missing variables", {

  skip_if_not_installed("survey")

  df <- make_validation_data()
  cfg <- make_validation_cfg()

  expect_error(
    svySE_calc(
      data = df,
      indicators = "missing_indicator",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "missing_group",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      strata = "missing_strata",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      cluster = "missing_cluster",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "missing_weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc validates scalar weight, division, and division weight", {

  skip_if_not_installed("survey")

  df <- make_validation_data()
  cfg <- make_validation_cfg()

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = c("weight", "div_weight"),
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      division = c("area", "dept"),
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      division = "area",
      div_weight = c("weight", "div_weight"),
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc validates group label length", {

  skip_if_not_installed("survey")

  df <- make_validation_data()
  cfg <- make_validation_cfg()

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = c("dept", "area"),
      group_labels = "Department",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc rejects missing values in grouping and design variables", {

  skip_if_not_installed("survey")

  cfg <- make_validation_cfg()

  df_group <- make_validation_data()
  df_group$dept[1] <- NA_character_

  expect_error(
    svySE_calc(
      data = df_group,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  df_strata <- make_validation_data()
  df_strata$strata[1] <- NA_character_

  expect_error(
    svySE_calc(
      data = df_strata,
      indicators = "ind_1",
      group_vars = "dept",
      strata = "strata",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  df_cluster <- make_validation_data()
  df_cluster$cluster[1] <- NA_integer_

  expect_error(
    svySE_calc(
      data = df_cluster,
      indicators = "ind_1",
      group_vars = "dept",
      cluster = "cluster",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc validates sampling weight values", {

  skip_if_not_installed("survey")

  cfg <- make_validation_cfg()

  df_negative <- make_validation_data()
  df_negative$weight[1] <- -1

  expect_error(
    svySE_calc(
      data = df_negative,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  df_missing <- make_validation_data()
  df_missing$weight <- NA_real_

  expect_error(
    svySE_calc(
      data = df_missing,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )

  df_zero <- make_validation_data()
  df_zero$weight <- 0

  expect_error(
    svySE_calc(
      data = df_zero,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc warns about zero and non-numeric weights", {

  skip_if_not_installed("survey")

  cfg <- make_validation_cfg()

  df_zero <- make_validation_data()
  df_zero$weight[1] <- 0

  expect_warning(
    svySE_calc(
      data = df_zero,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    "valores cero"
  )

  df_text <- make_validation_data()
  df_text$weight <- as.character(df_text$weight)
  df_text$weight[1] <- "invalid"

  expect_warning(
    result <- svySE_calc(
      data = df_text,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      verbose = FALSE
    ),
    "no numericos"
  )

  expect_s3_class(result, "svySE_result")
})


test_that("svySE_calc handles invalid indicator values according to strict", {

  skip_if_not_installed("survey")

  cfg <- make_validation_cfg()
  df <- make_validation_data()
  df$ind_1[1] <- 2

  expect_warning(
    result <- svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      strict = FALSE,
      verbose = FALSE
    ),
    "fuera de `valid_values`"
  )

  expect_s3_class(result, "svySE_result")

  expect_error(
    svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      cfg = cfg,
      strict = TRUE,
      verbose = FALSE
    ),
    class = "svySE_error"
  )
})


test_that("svySE_calc warns about missing division values", {

  skip_if_not_installed("survey")

  cfg <- make_validation_cfg()
  df <- make_validation_data()
  df$area[1] <- NA_character_

  expect_warning(
    result <- svySE_calc(
      data = df,
      indicators = "ind_1",
      group_vars = "dept",
      weight = "weight",
      division = "area",
      cfg = cfg,
      verbose = FALSE
    ),
    "valores perdidos"
  )

  expect_s3_class(result, "svySE_result")
})
