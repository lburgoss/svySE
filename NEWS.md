# svySE 0.2.0

## New features

* Added optional `cluster` argument in `svySE_calc()`.
* Added support for clustered survey designs using `survey::svydesign(ids = ...)`.
* Added support for unstratified designs by allowing `strata = NULL`.
* Added support for unclustered designs by allowing `cluster = NULL`.
* Added diagnostic warning for strata containing only one cluster.

## Changes

* `svySE_calc()` now supports four survey design structures:
  * weight only;
  * weight + strata;
  * weight + cluster;
  * weight + strata + cluster.

# svySE 0.1.0

## New features

* First public development release.
* Added configurable survey settings.
* Added sampling error estimation.
* Added weighted totals and proportions.
* Added confidence intervals.
* Added coefficients of variation.
* Added design effects.
* Added customizable Excel export.
* Added package vignettes.
* Added unit tests.
* Added GitHub Actions workflow.