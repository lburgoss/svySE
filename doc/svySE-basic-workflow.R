## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----example-data-------------------------------------------------------------
library(svySE)

set.seed(123)

df <- data.frame(
  dept = rep(c("A", "B", "C"), each = 50),
  strata = rep(c("S1", "S2", "S3"), each = 50),
  cluster = rep(seq_len(30), each = 5),
  service = rep(c("S1", "S2"), length.out = 150),
  weight = runif(150, 10, 50),
  ind_1 = sample(c(0, 1), 150, replace = TRUE),
  ind_2 = sample(c(0, 1), 150, replace = TRUE),
  stringsAsFactors = FALSE
)

head(df)

## ----configuration------------------------------------------------------------
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

cfg

## ----sampling-errors----------------------------------------------------------
res_error <- svySE_calc(
  data = df,
  indicators = c("ind_1", "ind_2"),
  group_vars = "dept",
  group_labels = "Department",
  strata = "strata",
  cluster = "cluster",
  weight = "weight",
  division = NULL,
  div_weight = NULL,
  cfg = cfg,
  verbose = FALSE
)

res_error

## ----sampling-error-class-----------------------------------------------------
class(res_error)

## ----inspect-error-object-----------------------------------------------------
names(res_error$results)
names(res_error$results$ind_1$error)

## ----inspect-error-table------------------------------------------------------
res_error$results$ind_1$error$TOTAL

## ----design-weight-only-------------------------------------------------------
res_weight <- svySE_calc(
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

res_weight

## ----design-stratified--------------------------------------------------------
res_strata <- svySE_calc(
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

res_strata

## ----design-clustered---------------------------------------------------------
res_cluster <- svySE_calc(
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

res_cluster

## ----design-complex-----------------------------------------------------------
res_complex <- svySE_calc(
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

res_complex

## ----domain-estimation--------------------------------------------------------
res_domain <- svySE_calc(
  data = df,
  indicators = "ind_1",
  group_vars = "dept",
  group_labels = "Department",
  strata = "strata",
  cluster = "cluster",
  weight = "weight",
  division = "service",
  div_weight = NULL,
  cfg = cfg,
  verbose = FALSE
)

names(res_domain$results$ind_1$error)

## ----inspect-domain-----------------------------------------------------------
res_domain$results$ind_1$error$S1

## ----simple-tables------------------------------------------------------------
res_simple <- svySE_simple(
  data = df,
  indicators = c("ind_1", "ind_2"),
  group_vars = "dept",
  group_labels = "Department",
  division = NULL,
  target = 1,
  valid_values = c(0, 1),
  pct_mult = 100,
  verbose = FALSE
)

res_simple

## ----simple-class-------------------------------------------------------------
class(res_simple)

## ----inspect-simple-----------------------------------------------------------
res_simple$results$ind_1$simple$TOTAL

## ----simple-domain------------------------------------------------------------
res_simple_domain <- svySE_simple(
  data = df,
  indicators = "ind_1",
  group_vars = "dept",
  group_labels = "Department",
  division = "service",
  target = 1,
  valid_values = c(0, 1),
  pct_mult = 100,
  verbose = FALSE
)

names(res_simple_domain$results$ind_1$simple)

## ----export-error-------------------------------------------------------------
file_err <- tempfile(fileext = ".xlsx")

export_error <- svySE_xlsx(
  x = res_error,
  file_err = file_err,
  file_tab = NULL,
  cols_err = svySE_cols_err("full"),
  overwrite = TRUE
)

file.exists(file_err)

## ----export-simple------------------------------------------------------------
file_tab <- tempfile(fileext = ".xlsx")

export_simple <- svySE_xlsx(
  x = res_simple,
  file_err = NULL,
  file_tab = file_tab,
  cols_tab = svySE_cols_tab("full"),
  overwrite = TRUE
)

file.exists(file_tab)

## ----multiple-results---------------------------------------------------------
results <- list(
  Main_errors = res_error,
  Domain_errors = res_domain,
  Main_simple = res_simple,
  Domain_simple = res_simple_domain
)

## ----export-multiple----------------------------------------------------------
multiple_err <- tempfile(fileext = ".xlsx")
multiple_tab <- tempfile(fileext = ".xlsx")

export_multiple <- svySE_xlsx(
  x = results,
  file_err = multiple_err,
  file_tab = multiple_tab,
  cols_err = svySE_cols_err("full"),
  cols_tab = svySE_cols_tab("full"),
  overwrite = TRUE
)

file.exists(multiple_err)
file.exists(multiple_tab)

## ----select-errors------------------------------------------------------------
selected_err <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = results,
  select = c("Main_errors", "Domain_errors"),
  file_err = selected_err,
  file_tab = NULL,
  overwrite = TRUE
)

file.exists(selected_err)

## ----select-simple------------------------------------------------------------
selected_tab <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = results,
  select = c("Main_simple", "Domain_simple"),
  file_err = NULL,
  file_tab = selected_tab,
  overwrite = TRUE
)

file.exists(selected_tab)

## ----error-columns-full-------------------------------------------------------
svySE_cols_err("full")

## ----error-columns-custom-----------------------------------------------------
error_columns <- svySE_cols_err(
  type = "custom",
  cols = c(
    "est_pct",
    "se_pct",
    "ci_l_pct",
    "ci_u_pct",
    "cv",
    "deff",
    "n_unw"
  )
)

error_columns

## ----export-custom-errors-----------------------------------------------------
custom_err <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = res_error,
  file_err = custom_err,
  file_tab = NULL,
  cols_err = error_columns,
  overwrite = TRUE
)

file.exists(custom_err)

## ----simple-columns-full------------------------------------------------------
svySE_cols_tab("full")

## ----simple-columns-custom----------------------------------------------------
simple_columns <- svySE_cols_tab(
  type = "custom",
  cols = c(
    "freq_1",
    "pct_1",
    "freq_total"
  )
)

simple_columns

## ----export-custom-simple-----------------------------------------------------
custom_tab <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = res_simple,
  file_err = NULL,
  file_tab = custom_tab,
  cols_tab = simple_columns,
  overwrite = TRUE
)

file.exists(custom_tab)

