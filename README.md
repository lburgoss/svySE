<p align="center">
  <img src="man/figures/logo.png" width="260" alt="svySE logo">
</p>

# svySE

[![R-CMD-check](https://github.com/lburgoss/svySE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lburgoss/svySE/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/svySE)](https://CRAN.R-project.org/package=svySE)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/svySE)](https://cran.r-project.org/package=svySE)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

**Sampling Error Estimation for Complex Surveys**

`svySE` is an R package for estimating sampling errors, producing descriptive indicator tables, and exporting structured results from complex survey data.

The package provides two complementary workflows:

- `svySE_calc()` calculates weighted estimates and sampling errors using the survey design.
- `svySE_simple()` calculates unweighted frequencies and percentages from the observed sample.

Results can be exported individually or consolidated across multiple datasets, survey weights, indicators, or analysis runs using `svySE_xlsx()`.

`svySE` is built on top of the **survey** package and provides a higher-level workflow for the routine production of survey indicators while preserving the principles of design-based estimation.

---

## Why svySE?

Survey indicator production often involves repeating the same technical steps:

1. preparing indicator variables;
2. identifying grouping and domain variables;
3. defining the survey design;
4. calculating weighted estimates;
5. estimating standard errors and confidence intervals;
6. calculating CV and DEFF;
7. preparing descriptive tables;
8. exporting results to structured workbooks.

`svySE` organizes these tasks into a reproducible and configurable workflow.

The package is designed to be:

- reproducible;
- flexible;
- easy to configure;
- suitable for repeated indicator production;
- compatible with complex survey designs;
- useful for official statistics;
- useful for academic and applied research;
- suitable for analyses involving multiple datasets or expansion factors.

Although the package was developed from practical experience in survey sampling and official statistics, it can be used by any researcher, analyst, public institution, national statistical office, or survey practitioner working with indicator-based survey data.

---

## Main Features

- Weighted totals
- Weighted proportions
- Standard errors
- Confidence intervals
- Coefficients of variation
- Design effects
- Unweighted sample sizes
- Grouped estimates
- Domain estimates
- Optional strata variables
- Optional cluster variables
- Unstratified designs
- Unclustered designs
- Unweighted simple indicator tables
- Flexible column selection
- Consolidated export of multiple analyses
- Selective export of chosen results
- Customizable `.xlsx` outputs

---

## Package Workflow

| Step | Function | Purpose |
|------|----------|---------|
| **1** | `svySE_cfg()` | Configure estimation settings, confidence level, target category, CV, DEFF, and other options. |
| **2A** | `svySE_calc()` | Calculate weighted estimates and sampling errors using the survey design. |
| **2B** | `svySE_simple()` | Calculate unweighted frequencies and percentages from the observed sample. |
| **3** | `svySE_xlsx()` | Export one or multiple results to `.xlsx` files. |

The two calculation functions have separate responsibilities:

| Function | Uses weights | Uses survey design | Main output |
|----------|:------------:|:------------------:|-------------|
| `svySE_calc()` | Yes | Yes | Weighted estimates and sampling errors |
| `svySE_simple()` | No | No | Unweighted frequencies and percentages |

This separation avoids redundant calculations and allows users to run only the workflow required for each analysis.

---

## Installation

### Stable version from CRAN

```r
install.packages("svySE")
```

### Development version from GitHub

```r
install.packages("remotes")

remotes::install_github("lburgoss/svySE")
```

The CRAN version is recommended for regular use. The GitHub version may contain features under development before they are submitted to CRAN.

Load the package with:

```r
library(svySE)
```

---

## Example Data

The following simulated dataset contains:

- a geographic grouping variable;
- a stratification variable;
- a cluster variable;
- a sampling weight;
- a division variable;
- two binary indicators.

```r
library(svySE)

set.seed(123)

df <- data.frame(
  dept = rep(c("A", "B", "C"), each = 50),
  strata = rep(c("S1", "S2", "S3"), each = 50),
  cluster = rep(1:30, each = 5),
  service = rep(c("S1", "S2"), length.out = 150),
  weight = runif(150, 10, 50),
  ind_1 = sample(c(0, 1), 150, replace = TRUE),
  ind_2 = sample(c(0, 1), 150, replace = TRUE)
)

head(df)
```

---

## Configure the Analysis

`svySE_cfg()` defines the common settings used during sampling error estimation.

```r
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
```

The most relevant options are:

| Argument | Description |
|----------|-------------|
| `estimator` | Estimator used in the analysis, such as `"prop"` or `"total"`. |
| `variance` | Variance estimation method. |
| `lonely_psu` | Treatment of strata containing a single PSU. |
| `conf_level` | Confidence level used for interval estimation. |
| `target` | Indicator category treated as the target value. |
| `valid_values` | Values considered valid for the indicator. |
| `truncate_lower_ci` | Whether lower confidence limits are truncated at zero. |
| `pct_mult` | Multiplier used to express percentages. |
| `deff` | Whether design effects are calculated. |
| `cv` | Whether coefficients of variation are calculated. |
| `na_rm` | Whether missing values are removed during estimation. |

---

## Calculate Sampling Errors

`svySE_calc()` estimates weighted indicators and sampling errors using the survey design.

```r
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
```

The result is an object of class:

```r
class(res_error)
```

A specific sampling error table can be inspected with:

```r
res_error$results$ind_1$error$TOTAL
```

The output may contain:

| Column | Description |
|--------|-------------|
| `est_abs` | Weighted absolute estimate |
| `est_pct` | Weighted percentage estimate |
| `se_abs` | Standard error of the absolute estimate |
| `se_pct` | Standard error of the percentage |
| `ci_l_abs` | Lower confidence limit for the absolute estimate |
| `ci_l_pct` | Lower confidence limit for the percentage |
| `ci_u_abs` | Upper confidence limit for the absolute estimate |
| `ci_u_pct` | Upper confidence limit for the percentage |
| `cv` | Coefficient of variation |
| `deff` | Design effect |
| `n_unw` | Unweighted count of target cases |

---

## Supported Survey Designs

`svySE_calc()` supports several survey design structures.

| Design structure | `strata` | `cluster` |
|------------------|----------|-----------|
| Weight only | `NULL` | `NULL` |
| Stratified design | Variable name | `NULL` |
| Clustered design | `NULL` | Variable name |
| Stratified clustered design | Variable name | Variable name |

### Weight only

```r
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
```

### Stratified design

```r
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
```

### Clustered design

```r
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
```

### Stratified clustered design

```r
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
```

---

## Domain Estimation

A division variable can be used to calculate separate results for its categories while retaining the design-based estimation workflow.

```r
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
```

Available divisions can be inspected with:

```r
names(res_domain$results$ind_1$error)
```

When `div_weight` is supplied, that weight is used for the corresponding division estimates.

---

## Simple Indicator Tables

`svySE_simple()` calculates frequencies and percentages without using sampling weights, strata, or cluster variables.

```r
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
```

A specific table can be inspected with:

```r
res_simple$results$ind_1$simple$TOTAL
```

The output includes:

| Column | Description |
|--------|-------------|
| `freq_0` | Frequency of non-target cases |
| `pct_0` | Percentage of non-target cases |
| `freq_1` | Frequency of target cases |
| `pct_1` | Percentage of target cases |
| `freq_total` | Total number of valid observations |
| `pct_total` | Total percentage |

> The results produced by `svySE_simple()` describe only the observed sample. Because no sampling weights or survey design variables are used, these percentages should not be interpreted as population estimates.

---

## Simple Tables by Division

```r
res_simple_domain <- svySE_simple(
  data = df,
  indicators = "ind_1",
  group_vars = "dept",
  group_labels = "Department",
  division = "service",
  target = 1,
  valid_values = c(0, 1),
  verbose = FALSE
)
```

Available divisions can be reviewed with:

```r
names(res_simple_domain$results$ind_1$simple)
```

---

## Export Results to XLSX

`svySE_xlsx()` exports sampling error results, simple indicator tables, or both.

### Export one sampling error result

```r
file_err <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = res_error,
  file_err = file_err,
  file_tab = NULL,
  cols_err = svySE_cols_err("full"),
  overwrite = TRUE
)

file.exists(file_err)
```

### Export one simple result

```r
file_tab <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = res_simple,
  file_err = NULL,
  file_tab = file_tab,
  cols_tab = svySE_cols_tab("full"),
  overwrite = TRUE
)

file.exists(file_tab)
```

---

## Export Multiple Analyses

Multiple results generated from different datasets, indicators, survey weights, or function calls can be exported together.

```r
results <- list(
  Main_errors = res_error,
  Domain_errors = res_domain,
  Main_simple = res_simple,
  Domain_simple = res_simple_domain
)
```

Export all available results:

```r
file_err <- tempfile(fileext = ".xlsx")
file_tab <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = results,
  file_err = file_err,
  file_tab = file_tab,
  cols_err = svySE_cols_err("full"),
  cols_tab = svySE_cols_tab("full"),
  overwrite = TRUE
)
```

`svySE_xlsx()` automatically identifies:

- objects generated by `svySE_calc()`;
- objects generated by `svySE_simple()`.

Sampling error tables are written to `file_err`, while simple indicator tables are written to `file_tab`.

---

## Export Selected Results

The `select` argument can be used to export only chosen elements from a named list.

```r
svySE_xlsx(
  x = results,
  select = c("Main_errors", "Domain_errors"),
  file_err = tempfile(fileext = ".xlsx"),
  file_tab = NULL,
  overwrite = TRUE
)
```

Export only simple tables:

```r
svySE_xlsx(
  x = results,
  select = c("Main_simple", "Domain_simple"),
  file_err = NULL,
  file_tab = tempfile(fileext = ".xlsx"),
  overwrite = TRUE
)
```

---

## Customize Exported Columns

### Sampling error columns

```r
svySE_cols_err("full")
```

A custom selection can be defined with:

```r
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
```

Use the selected columns during export:

```r
svySE_xlsx(
  x = res_error,
  file_err = tempfile(fileext = ".xlsx"),
  file_tab = NULL,
  cols_err = error_columns
)
```

### Simple table columns

```r
svySE_cols_tab("full")
```

A custom selection can be defined with:

```r
simple_columns <- svySE_cols_tab(
  type = "custom",
  cols = c(
    "freq_1",
    "pct_1",
    "freq_total"
  )
)
```

Use the selected columns during export:

```r
svySE_xlsx(
  x = res_simple,
  file_err = NULL,
  file_tab = tempfile(fileext = ".xlsx"),
  cols_tab = simple_columns
)
```

---

## Main Functions

| Function | Description |
|----------|-------------|
| `svySE_cfg()` | Configure sampling error estimation settings |
| `svySE_calc()` | Calculate weighted estimates and sampling errors |
| `svySE_simple()` | Calculate unweighted frequencies and percentages |
| `svySE_xlsx()` | Export one or multiple results to `.xlsx` files |
| `svySE_cols_err()` | Select sampling error columns |
| `svySE_cols_tab()` | Select simple table columns |

---

## Output Types

| Output | Generated by | Weighted | Uses survey design | Typical use |
|--------|--------------|:--------:|:------------------:|-------------|
| Sampling error tables | `svySE_calc()` | Yes | Yes | Official statistics, complex surveys, technical reports |
| Simple indicator tables | `svySE_simple()` | No | No | Descriptive analysis and sample-level reporting |

---

## Technical Ecosystem

`svySE` integrates functionality from established R packages.

| Package | Role |
|---------|------|
| `survey` | Design-based estimation, standard errors, confidence intervals, CV, and DEFF |
| `openxlsx` | Creation and formatting of `.xlsx` workbooks |
| `stats` | Statistical formulas, coefficients, and confidence intervals |
| `svySE` | High-level workflow for survey indicators, errors, tables, and export |

`svySE` does not replace `survey`. It provides a structured interface for repeated indicator production and export workflows built on top of its design-based estimation capabilities.

---

## Documentation

The package includes:

- a reference manual;
- function documentation;
- package vignettes;
- reproducible examples;
- unit tests.

Open the package help:

```r
help(package = "svySE")
```

Browse available vignettes:

```r
browseVignettes("svySE")
```

Open documentation for the principal functions:

```r
?svySE_cfg
?svySE_calc
?svySE_simple
?svySE_xlsx
```

---

## Development Status

Version **0.2.0** introduces:

- optional cluster variables;
- support for unstratified and unclustered designs;
- the new `svySE_simple()` workflow;
- separation between weighted estimation and descriptive simple tables;
- consolidated export of multiple analyses;
- selective export through the `select` argument;
- customizable `.xlsx` workbooks.

Future releases will focus on additional estimators, expanded quality indicators, more export options, and broader support for complex survey workflows.

---

## Author

**Luis Burgos**

Statistician • RENACYT Researcher (Peru)

Sampling Specialist

National Institute of Statistics and Informatics (INEI)

ENCAL — Public Expenditure Quality Monitoring Survey

The package was developed independently based on professional experience in complex survey sampling, official statistics, and statistical programming.

Email: lburgoss1996@gmail.com

Suggestions, bug reports, and feature requests are welcome through the GitHub issue tracker.

---

## License

MIT License
