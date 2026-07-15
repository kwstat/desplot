# desplot 1.11 ()

* `ggdesplot()` no longer draws a spurious `no_color` legend when `text` or `num` is used without `col`. (P.Schmidt)

* `ggdesplot()` now facets on every conditioning variable in a formula such as `yield ~ col*row | site + rep`. Previously only the first was used and the others were silently dropped, overplotting cells. (P.Schmidt)

* `ggdesplot()` named `col.regions` and `col.text` now fall back to positional matching (as the warning states) when some factor levels are unnamed, instead of leaving cells uncolored. (P.Schmidt)

* Switch to MIT license.

* Documentation pages now created via Github Actions.

* Fix the checking for multiple cell values when there are 2 panel factors.

* New support for `aspect` with `ggdesplot()`. (P.Schmidt)

* New support for named colors (P.Schmidt).

* Deprecated syntax `desplot(formula,data)`.

# desplot 1.10 (2023-03-01)

* One-row panels no longer have whitespace. Issue #9.

* Replace `aes_string(x=x.string)` with `aes(x=.data[[x.string]])` etc.


# desplot 1.9 (2021-10-30)

* Tweaks to `ggdesplot` output.

* Remove LazyData from DESCRIPTION.


# desplot 1.8 (2020-10-21)

* Bug fix for `dq` with multiple panels.

* Use `inherits` to check class #4.


# desplot 1.7 (2020-07-20)

* Please use `desplot(data,formula)` instead of `desplot(formula,data)`.


# desplot 1.6 (2019-09-13)

* New argument `dq` for showing data quality on heatmaps.


# desplot 1.5 (2019-04-04)

* Beta version of `ggdesplot()` to create `ggplot2` graphics.

* New argument `subset` to subset data before analysis.


# desplot 1.3 (2017-10-13)

* Bug fix.


# desplot 1.2 (2017-07-13)

* Now using `testthat` and `covr` packages.


# desplot 1.1 (2016-12-18)

* New function argument `midpoint="median"` uses the median to determine the midpoint for the ribbon. Previously, the midpoint was halfway between the minimum and maximum data values.


# desplot 1.0 (2015-12-14)

* The `desplot` package has been split off from the `agridat` package.


# desplot 0.0 (2008)

* Original creation of function.
