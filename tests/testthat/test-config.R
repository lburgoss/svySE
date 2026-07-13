# ==============================================================================
# Tests: configuracion
# Tests: configuration
# Archivo / File: tests/testthat/test-config.R
# ==============================================================================


test_that("svySE_cfg creates a valid configuration object", {

  old_option <- getOption("survey.lonely.psu")
  on.exit(options(survey.lonely.psu = old_option), add = TRUE)

  cfg <- svySE_cfg(
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

  expect_s3_class(cfg, "svySE_cfg")
  expect_true(svySE_is_cfg(cfg))

  expect_equal(cfg$estimator, "prop")
  expect_equal(cfg$variance, "taylor")
  expect_equal(cfg$lonely_psu, "adjust")
  expect_equal(cfg$conf_level, 0.95)
  expect_equal(cfg$target, 1)
  expect_equal(cfg$valid_values, c(0, 1))
  expect_true(cfg$truncate_lower_ci)
  expect_equal(cfg$pct_mult, 100)
  expect_true(cfg$deff)
  expect_true(cfg$cv)
  expect_true(cfg$na_rm)

  expect_equal(getOption("survey.lonely.psu"), "adjust")
})


test_that("svySE_cfg accepts the supported estimators and lonely PSU options", {

  old_option <- getOption("survey.lonely.psu")
  on.exit(options(survey.lonely.psu = old_option), add = TRUE)

  estimators <- c("prop", "total", "mean", "ratio")
  lonely_options <- c("adjust", "average", "certainty", "remove", "fail")

  for (estimator in estimators) {
    cfg <- svySE_cfg(estimator = estimator)
    expect_equal(cfg$estimator, estimator)
  }

  for (option in lonely_options) {
    cfg <- svySE_cfg(lonely_psu = option)
    expect_equal(cfg$lonely_psu, option)
    expect_equal(getOption("survey.lonely.psu"), option)
  }
})


test_that("svySE_cfg validates estimator, variance, and lonely PSU values", {

  expect_error(
    svySE_cfg(estimator = "invalid"),
    "arg"
  )

  expect_error(
    svySE_cfg(variance = "bootstrap"),
    "arg"
  )

  expect_error(
    svySE_cfg(lonely_psu = "invalid"),
    "arg"
  )
})


test_that("svySE_cfg validates confidence level and percentage multiplier", {

  expect_error(
    svySE_cfg(conf_level = "0.95"),
    "conf_level"
  )

  expect_error(
    svySE_cfg(conf_level = c(0.90, 0.95)),
    "conf_level"
  )

  expect_error(
    svySE_cfg(conf_level = 0),
    "conf_level"
  )

  expect_error(
    svySE_cfg(conf_level = 1),
    "conf_level"
  )

  expect_error(
    svySE_cfg(conf_level = NA_real_),
    "conf_level"
  )

  expect_error(
    svySE_cfg(pct_mult = "100"),
    "pct_mult"
  )

  expect_error(
    svySE_cfg(pct_mult = c(100, 1)),
    "pct_mult"
  )

  expect_error(
    svySE_cfg(pct_mult = 0),
    "pct_mult"
  )

  expect_error(
    svySE_cfg(pct_mult = NA_real_),
    "pct_mult"
  )
})


test_that("svySE_cfg validates target and valid values", {

  expect_error(
    svySE_cfg(target = c(0, 1)),
    "target"
  )

  expect_error(
    svySE_cfg(valid_values = NULL),
    "valid_values"
  )

  expect_error(
    svySE_cfg(valid_values = numeric(0)),
    "valid_values"
  )

  expect_error(
    svySE_cfg(target = 2, valid_values = c(0, 1)),
    "target"
  )
})


test_that("svySE_cfg validates logical arguments", {

  expect_error(
    svySE_cfg(truncate_lower_ci = 1),
    "truncate_lower_ci"
  )

  expect_error(
    svySE_cfg(deff = NA),
    "deff"
  )

  expect_error(
    svySE_cfg(cv = c(TRUE, FALSE)),
    "cv"
  )

  expect_error(
    svySE_cfg(na_rm = "TRUE"),
    "na_rm"
  )
})


test_that("print.svySE_cfg returns the configuration invisibly", {

  cfg <- svySE_cfg()

  output <- capture.output(
    returned <- print(cfg)
  )

  expect_identical(returned, cfg)
  expect_true(any(grepl("svySE configuration", output, fixed = TRUE)))
  expect_true(any(grepl("Estimator", output, fixed = TRUE)))
})
