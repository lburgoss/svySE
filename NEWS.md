# svySE 0.2.1

## New features

* Added the `na_rm` argument to `svySE_simple()` for explicit handling of
  missing indicator values.
* Added optional weighted simple tables through the `weight` argument in
  `svySE_simple()`.
* Added the `output` argument to `svySE_simple()` with the following modes:
  `"unweighted"`, `"weighted"`, and `"both"`.
* Added expanded frequency and weighted percentage columns:
  `exp_0`, `exp_pct_0`, `exp_1`, `exp_pct_1`, `exp_total`, and
  `exp_pct_total`.
* Added new simple-table column profiles in `svySE_cols_tab()`:
  `"unweighted"`, `"expanded"`, `"expanded_freq"`, `"expanded_pct"`,
  `"counts"`, and `"percentages"`.
* Added automatic detection of available simple-table columns during XLSX
  export when `cols_tab = NULL`.

## Improvements

* `svySE_simple()` now omits groups without valid indicator records when
  `na_rm = TRUE`.
* Added informative validation when missing indicator values are present and
  `na_rm = FALSE`.
* Improved the handling of indicator-specific grouping structures in simple
  tables.
* Groups are now determined after filtering valid records for each indicator.
* Added validation when requested XLSX columns are not available in the
  calculated simple-table object.
* Optimized survey-design construction by avoiding unnecessary PSU nesting
  when no cluster variable is supplied. This substantially reduces computation
  time for unclustered designs while preserving equivalent statistical
  results.
* Expanded unit tests for weighted frequencies, weighted percentages, output
  modes, missing values, column profiles, and XLSX export.
* Updated the README, vignette, documentation, and examples.

# svySE 0.2.0

## New features

* Added `svySE_simple()` for calculating unweighted frequencies and percentages without sampling weights or survey design variables.
* Added optional `cluster` support in `svySE_calc()`.
* Added support for unstratified and unclustered survey designs.
* Added consolidated `.xlsx` export for multiple results from different data sources, weights, and function calls.
* Added the `select` argument in `svySE_xlsx()` to export only selected results.

## Changes

* `svySE_calc()` now calculates sampling estimates and sampling errors only.
* Simple unweighted tables are now generated exclusively with `svySE_simple()`.
* Removed redundant simple-table calculations from `svySE_calc()`.
* `svySE_xlsx()` now accepts:
  * one `svySE_result`;
  * one `svySE_simple_result`;
  * a named list containing several result objects.
* Sampling error tables and simple tables are exported to separate `.xlsx` files.
* Updated `DESCRIPTION` according to CRAN formatting recommendations.
* Updated installation instructions to include the CRAN version.

# svySE 0.1.0

## New features

* First public development release.
* Added configurable survey settings.
* Added sampling error estimation.
* Added weighted totals and proportions.
* Added confidence intervals.
* Added coefficients of variation.
* Added design effects.
* Added customizable XLSX export.
* Added package vignettes.
* Added unit tests.
* Added GitHub Actions workflow.