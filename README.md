<p align="center">
  <img src="man/figures/logo.png" width="260">
</p>

# svySE

[![R-CMD-check](https://github.com/lburgoss/svySE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lburgoss/svySE/actions/workflows/R-CMD-check.yaml)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

**Sampling Error Estimation for Complex Surveys**

`svySE` is an R package designed to estimate sampling errors and produce indicator tables from complex survey designs.

The package provides a reproducible workflow for calculating weighted totals, proportions, standard errors, confidence intervals, coefficients of variation (CV), design effects (DEFF), unweighted sample sizes, grouped estimates, domain estimates, and customizable Excel reports.

Built on top of the **survey** package, `svySE` simplifies the routine production of official survey indicators while preserving the methodological principles of design-based estimation.

---

# Why svySE?

Many organizations repeatedly implement similar survey estimation procedures using custom scripts.

`svySE` was developed to standardize these procedures into a single workflow that is:

- Reproducible
- Flexible
- Easy to configure
- Methodologically consistent
- Suitable for official statistics
- Suitable for academic research

Although initially developed from practical experience in official statistics, the package is intended for any researcher or institution working with complex survey data.

---

# Main Features

✔ Complex survey estimation using the **survey** package

✔ Weighted totals

✔ Weighted proportions

✔ Standard errors

✔ Confidence intervals

✔ Coefficients of variation (CV)

✔ Design effects (DEFF)

✔ Unweighted sample sizes

✔ Domain estimation

✔ Grouped estimation

✔ Configurable survey settings

✔ Customizable Excel exports

---

# Workflow

The typical workflow consists of only three steps.

```text
Configure survey estimation
        │
        ▼
   svySE_cfg()
        │
        ▼
Calculate sampling errors
        │
        ▼
  svySE_calc()
        │
        ▼
Export results
        │
        ▼
  svySE_xlsx()
```

---

# Installation

Development version

```r
install.packages("remotes")

remotes::install_github("lburgoss/svySE")
```

---

# Basic Example

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
  valid_values = c(0,1),
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

print(res)
```

---

# Export Results

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

---

# Main Functions

| Function | Description |
|----------|-------------|
| `svySE_cfg()` | Configure survey estimation settings |
| `svySE_calc()` | Calculate sampling errors |
| `svySE_xlsx()` | Export results to Excel |
| `svySE_cols_err()` | Select error table columns |
| `svySE_cols_tab()` | Select simple table columns |

---

# Documentation

The package includes:

- Reference manual
- Package vignettes
- Function documentation
- Examples
- Unit tests

Complete documentation is available directly in R:

```r
help(package = "svySE")
```

or

```r
browseVignettes("svySE")
```

---

# Development Status

`svySE` is under active development.

Future releases will incorporate additional estimators for complex survey analysis while maintaining compatibility with the **survey** package.

---

# Author

**Luis Burgos**

Statistician • Master's degree in Computer Science • RENACYT Researcher (Peru)

Sampling Specialist

National Institute of Statistics and Informatics (INEI)

ENCAL — Public Expenditure Quality Monitoring Survey

The package was developed independently based on professional experience in complex survey sampling, official statistics, and statistical programming.

📧 lburgoss1996@gmail.com

Suggestions, bug reports, and feature requests are welcome through the GitHub issue tracker.

---

# License

MIT License