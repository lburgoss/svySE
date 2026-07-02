# svySE

`svySE` is an R package for estimating sampling errors for complex survey indicators.

It supports weighted estimates, percentages, standard errors, confidence intervals, coefficients of variation, design effects, unweighted counts, grouped estimates, optional division variables, and customizable Excel exports.

## Installation

```r
# Development version
remotes::install_github("lburgoss1996/svySE")
```

## Basic example

```r
library(svySE)

set.seed(123)

df <- data.frame(
  dept = rep(c("A", "B", "C"), each = 50),
  strata = rep(c("A", "B", "C"), each = 50),
  weight = runif(150, 10, 50),
  ind_1 = sample(c(0, 1), 150, replace = TRUE)
)

cfg <- svySE_cfg(
  estimator = "prop",
  target = 1,
  valid_values = c(0, 1),
  lonely_psu = "adjust"
)

res <- svySE_calc(
  data = df,
  indicators = "ind_1",
  group_vars = "dept",
  group_labels = "Department",
  strata = "strata",
  weight = "weight",
  cfg = cfg
)

res
```

## Export to Excel

```r
tmp_err <- tempfile(fileext = ".xlsx")
tmp_tab <- tempfile(fileext = ".xlsx")

svySE_xlsx(
  x = res,
  file_err = tmp_err,
  file_tab = tmp_tab,
  cols_err = svySE_cols_err("full"),
  cols_tab = svySE_cols_tab("full")
)
```

## Main functions

- `svySE_cfg()`: creates a calculation configuration.
- `svySE_calc()`: calculates sampling errors.
- `svySE_xlsx()`: exports results to Excel.
- `svySE_cols_err()`: selects error table columns.
- `svySE_cols_tab()`: selects simple table columns.

## License

MIT