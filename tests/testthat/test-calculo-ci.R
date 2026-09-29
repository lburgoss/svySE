# ==============================================================================
# Tests: metodos de intervalo de confianza para proporciones
# Tests: confidence interval methods for proportions
# Archivo / File: tests/testthat/test-calculo-ci.R
# ==============================================================================


make_ci_data <- function() {

  set.seed(20260929)

  n <- 1200

  data <- data.frame(
    dept = sample(c("A", "B", "C", "D"), n, TRUE, prob = c(0.5, 0.3, 0.17, 0.03)),
    strata = rep(sprintf("S%02d", 1:10), length.out = n),
    weight = runif(n, 5, 80),
    stringsAsFactors = FALSE
  )

  data$cluster <- paste(data$strata, sample(1:8, n, TRUE))
  data$ind_1 <- rbinom(n, 1, ifelse(data$dept == "D", 0.05, 0.25))
  data$ind_0 <- 1 - data$ind_1
  data$ind_zero <- ifelse(data$dept == "C", 0, data$ind_1)

  data
}


run_ci <- function(data, indicators, ci_method, target = 1, ...) {
  svySE_calc(
    data = data,
    indicators = indicators,
    group_vars = "dept",
    strata = "strata",
    cluster = "cluster",
    weight = "weight",
    cfg = svySE_cfg(estimator = "prop", target = target, ci_method = ci_method, ...),
    verbose = FALSE
  )
}


# ------------------------------------------------------------------------------
# Referencia IBM SPSS Complex Samples
# ------------------------------------------------------------------------------

test_that("svySE_ci_xlogit reproduces the IBM SPSS Complex Samples reference", {

  # SPSS reporta p = 1/34 y su complemento 33/34, SE = 0.0167321.
  spss <- list(
    p = c(0.0294118, 0.9705882),
    se = 0.0167321,
    lower = c(0.0095108, 0.9127153),
    upper = c(0.087284666, 0.990489152)
  )

  # El valor critico implicito en SPSS (~1.9608) corresponde a una t con
  # varios miles de gl de diseno; para cualquier gl >= 1000 la diferencia con
  # SPSS es menor a 1e-4.
  for (df in c(1000, 3000, Inf)) {
    for (i in 1:2) {
      ci <- svySE_ci_xlogit(spss$p[i], spss$se, 0.95, df)
      expect_lt(abs(ci[1] - spss$lower[i]), 1e-4)
      expect_lt(abs(ci[2] - spss$upper[i]), 1e-4)
    }
  }

  # Con gl = 3000 la coincidencia es practicamente exacta.
  ci <- svySE_ci_xlogit(1 / 34, spss$se, 0.95, 3000)
  expect_lt(max(abs(ci - c(spss$lower[1], spss$upper[1]))), 1e-6)
})


test_that("svySE_ci_xlogit matches the logit formula and is complementary", {

  p <- 0.12
  se <- 0.03
  df <- 25

  q <- stats::qt(0.975, df)
  se_eta <- se / (p * (1 - p))
  expected <- stats::plogis(stats::qlogis(p) + c(-1, 1) * q * se_eta)

  ci <- svySE_ci_xlogit(p, se, 0.95, df)
  ci_comp <- svySE_ci_xlogit(1 - p, se, 0.95, df)

  expect_equal(ci, expected)
  expect_equal(ci_comp, c(1 - ci[2], 1 - ci[1]))
  expect_true(ci[1] < p && p < ci[2])
})


test_that("svySE_ci_xlogit handles special cases", {

  # SE = 0 y extremos 0 / 1: intervalo degenerado [p, p].
  expect_equal(svySE_ci_xlogit(0, 0, 0.95, 30), c(0, 0))
  expect_equal(svySE_ci_xlogit(1, 0, 0.95, 30), c(1, 1))
  expect_equal(svySE_ci_xlogit(0.4, 0, 0.95, 30), c(0.4, 0.4))
  expect_equal(svySE_ci_xlogit(0, 0.01, 0.95, 30), c(0, 0))

  # Valores faltantes o invalidos: NA.
  na2 <- c(NA_real_, NA_real_)
  expect_equal(svySE_ci_xlogit(NA_real_, 0.01, 0.95, 30), na2)
  expect_equal(svySE_ci_xlogit(0.2, NA_real_, 0.95, 30), na2)
  expect_equal(svySE_ci_xlogit(0.2, 0.01, 0.95, NA_real_), na2)
  expect_equal(svySE_ci_xlogit(0.2, 0.01, 0.95, 0), na2)
  expect_equal(svySE_ci_xlogit(0.2, 0.01, 0.95, -3), na2)
  expect_equal(svySE_ci_xlogit(1.2, 0.01, 0.95, 30), na2)

  # Proporciones extremas: limites finitos, dentro de (0, 1) y ordenados.
  for (p in c(1e-10, 1e-6, 1 - 1e-6, 1 - 1e-10)) {
    ci <- svySE_ci_xlogit(p, sqrt(p * (1 - p) / 50), 0.95, 40)
    expect_true(all(is.finite(ci)))
    expect_true(ci[1] >= 0 && ci[1] <= p && p <= ci[2] && ci[2] <= 1)
  }

  # Pocos grados de libertad: intervalo mas amplio que con muchos.
  ci_small <- svySE_ci_xlogit(0.3, 0.05, 0.95, 2)
  ci_large <- svySE_ci_xlogit(0.3, 0.05, 0.95, Inf)
  expect_true(ci_small[1] < ci_large[1] && ci_small[2] > ci_large[2])
})


# ------------------------------------------------------------------------------
# Configuracion
# ------------------------------------------------------------------------------

test_that("svySE_cfg keeps wald as default and validates ci options", {

  old_option <- getOption("survey.lonely.psu")
  on.exit(options(survey.lonely.psu = old_option), add = TRUE)

  expect_equal(svySE_cfg()$ci_method, "wald")
  expect_null(svySE_cfg()$ci_df)
  expect_equal(svySE_cfg(ci_method = "xlogit")$ci_method, "xlogit")
  expect_equal(svySE_cfg(ci_method = "xlogit", ci_df = Inf)$ci_df, Inf)

  expect_error(svySE_cfg(ci_method = "wilson"), "arg")
  expect_error(svySE_cfg(estimator = "total", ci_method = "xlogit"), "xlogit")
  expect_error(svySE_cfg(estimator = "mean", ci_method = "xlogit"), "xlogit")
  expect_error(svySE_cfg(ci_df = 0), "ci_df")
  expect_error(svySE_cfg(ci_df = "10"), "ci_df")
  expect_error(svySE_cfg(ci_df = c(10, 20)), "ci_df")
  expect_error(svySE_cfg(ci_df = NA_real_), "ci_df")
})


# ------------------------------------------------------------------------------
# Integracion con svySE_calc
# ------------------------------------------------------------------------------

test_that("wald method keeps the historical normal-theory interval", {

  skip_if_not_installed("survey")

  tab <- run_ci(make_ci_data(), "ind_1", "wald")$results$ind_1$error$TOTAL

  z <- stats::qnorm(0.975)

  expect_equal(tab$ci_l_pct, pmax(tab$est_pct - z * tab$se_pct, 0))
  expect_equal(tab$ci_u_pct, tab$est_pct + z * tab$se_pct)
})


test_that("xlogit only changes the proportion interval", {

  skip_if_not_installed("survey")

  data <- make_ci_data()

  wald <- run_ci(data, "ind_1", "wald")$results$ind_1$error$TOTAL
  xlogit <- run_ci(data, "ind_1", "xlogit")$results$ind_1$error$TOTAL

  expect_identical(names(xlogit), names(wald))
  expect_identical(nrow(xlogit), nrow(wald))

  unchanged <- setdiff(names(wald), c("ci_l_pct", "ci_u_pct"))
  expect_identical(xlogit[unchanged], wald[unchanged])

  expect_false(isTRUE(all.equal(xlogit$ci_l_pct, wald$ci_l_pct)))
})


test_that("xlogit matches survey::svyciprop with the design degrees of freedom", {

  skip_if_not_installed("survey")

  data <- make_ci_data()
  tab <- run_ci(data, "ind_1", "xlogit")$results$ind_1$error$TOTAL

  design <- survey::svydesign(
    ids = ~cluster,
    strata = ~strata,
    weights = ~weight,
    data = data,
    nest = TRUE
  )

  df <- survey::degf(design)

  national <- survey::svyciprop(~I(ind_1 == 1), design, method = "xlogit", df = df)
  expect_equal(
    c(tab$ci_l_pct[1], tab$ci_u_pct[1]) / 100,
    as.numeric(stats::confint(national)),
    tolerance = 1e-10
  )

  # Dominios: SPSS y svySE usan los gl del diseno completo, no los del dominio.
  for (g in c("A", "B", "C", "D")) {
    dom <- survey::svyciprop(
      ~I(ind_1 == 1),
      subset(design, dept == g),
      method = "xlogit",
      df = df
    )

    row <- tab[tab$dept == g, ]

    expect_equal(row$est_pct / 100, as.numeric(dom), tolerance = 1e-10)
    expect_equal(
      c(row$ci_l_pct, row$ci_u_pct) / 100,
      as.numeric(stats::confint(dom)),
      tolerance = 1e-10
    )
  }
})


test_that("xlogit intervals are complementary across categories", {

  skip_if_not_installed("survey")

  data <- make_ci_data()

  tab_1 <- run_ci(data, "ind_1", "xlogit", target = 1)$results$ind_1$error$TOTAL
  tab_0 <- run_ci(data, "ind_1", "xlogit", target = 0)$results$ind_1$error$TOTAL

  expect_equal(tab_1$est_pct + tab_0$est_pct, rep(100, nrow(tab_1)))
  expect_equal(tab_1$se_pct, tab_0$se_pct)
  expect_equal(tab_0$ci_l_pct, 100 - tab_1$ci_u_pct)
  expect_equal(tab_0$ci_u_pct, 100 - tab_1$ci_l_pct)
})


test_that("xlogit handles empty categories and divisions", {

  skip_if_not_installed("survey")

  data <- make_ci_data()

  res <- suppressWarnings(
    svySE_calc(
      data = data,
      indicators = "ind_zero",
      group_vars = "dept",
      strata = "strata",
      cluster = "cluster",
      weight = "weight",
      division = "strata",
      cfg = svySE_cfg(estimator = "prop", ci_method = "xlogit"),
      verbose = FALSE
    )
  )

  tab <- res$results$ind_zero$error$TOTAL
  row_c <- tab[tab$dept == "C", ]

  expect_equal(row_c$est_pct, 0)
  expect_equal(row_c$ci_l_pct, 0)
  expect_equal(row_c$ci_u_pct, 0)

  for (div in res$results$ind_zero$error) {
    ok <- !is.na(div$est_pct)
    expect_true(all(div$ci_l_pct[ok] <= div$est_pct[ok] + 1e-12))
    expect_true(all(div$ci_u_pct[ok] >= div$est_pct[ok] - 1e-12))
    expect_true(all(div$ci_l_pct[ok] >= 0 & div$ci_u_pct[ok] <= 100))
  }
})


test_that("ci_df overrides the design degrees of freedom", {

  skip_if_not_installed("survey")

  data <- make_ci_data()

  tab_inf <- run_ci(data, "ind_1", "xlogit", ci_df = Inf)$results$ind_1$error$TOTAL
  tab_df5 <- run_ci(data, "ind_1", "xlogit", ci_df = 5)$results$ind_1$error$TOTAL

  expected <- svySE_ci_xlogit(tab_inf$est_pct[1] / 100, tab_inf$se_pct[1] / 100, 0.95, Inf)
  expect_equal(c(tab_inf$ci_l_pct[1], tab_inf$ci_u_pct[1]) / 100, expected)

  expect_true(all(tab_df5$ci_l_pct <= tab_inf$ci_l_pct))
  expect_true(all(tab_df5$ci_u_pct >= tab_inf$ci_u_pct))
})
