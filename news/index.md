# Changelog

## desplot 1.11 ()

- Switch to MIT license.

- Documentation pages now created via Github Actions.

- Fix the checking for multiple cell values when there are 2 panel
  factors.

- New support for `aspect` with
  [`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md).
  (P.Schmidt)

- New support for named colors (P.Schmidt).

## desplot 1.10 (2023-03-01)

CRAN release: 2023-03-09

- One-row panels no longer have whitespace. Issue
  [\#9](https://github.com/kwstat/desplot/issues/9).

- Replace `aes_string(x=x.string)` with `aes(x=.data[[x.string]])` etc.

## desplot 1.9 (2021-10-30)

CRAN release: 2021-10-29

- Tweaks to `ggdesplot` output.

- Remove LazyData from DESCRIPTION.

## desplot 1.8 (2020-10-21)

CRAN release: 2020-10-21

- Bug fix for `dq` with multiple panels.

- Use `inherits` to check class
  [\#4](https://github.com/kwstat/desplot/issues/4).

## desplot 1.7 (2020-07-20)

CRAN release: 2020-07-20

- Please use `desplot(data,formula)` instead of `desplot(formula,data)`.

## desplot 1.6 (2019-09-13)

CRAN release: 2019-09-13

- New argument `dq` for showing data quality on heatmaps.

## desplot 1.5 (2019-04-04)

CRAN release: 2019-04-03

- Beta version of
  [`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md)
  to create `ggplot2` graphics.

- New argument `subset` to subset data before analysis.

## desplot 1.3 (2017-10-13)

CRAN release: 2017-10-13

- Bug fix.

## desplot 1.2 (2017-07-13)

CRAN release: 2017-07-13

- Now using `testthat` and `covr` packages.

## desplot 1.1 (2016-12-18)

CRAN release: 2016-12-18

- New function argument `midpoint="median"` uses the median to determine
  the midpoint for the ribbon. Previously, the midpoint was halfway
  between the minimum and maximum data values.

## desplot 1.0 (2015-12-14)

CRAN release: 2015-12-14

- The `desplot` package has been split off from the `agridat` package.

## desplot 0.0 (2008)

- Original creation of function.
