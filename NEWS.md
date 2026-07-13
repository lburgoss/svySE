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