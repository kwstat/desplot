# desplot 1.10 - unpublished

* One row panels no longer have whitespace. Issue #9.

# desplot 1.9 - Oct 2021

* Tweaks to `ggdesplot` output.
* Remove LazyData from DESCRIPTION.


# desplot 1.8 - Oct 2020

* Bug fix for `dq` with multiple panels.
* Use `inherits` to check class #4.


# desplot 1.7 - Jul 2020

* Please use `desplot(data,formula)` instead of `desplot(formula,data)`.


# desplot 1.6 - Sep 2019

* New argument `dq` for showing data quality on heatmaps.


# desplot 1.5 - Apr 2019

* Beta version of `ggdesplot()` to create `ggplot2` graphics.

* New argument `subset` to subset data before analysis.


# desplot 1.3 - Oct 2017

* Bug fix.


# desplot 1.2 - Jul 2017

* Now using `testthat` and `covr` packages.


# desplot 1.1 - Dec 2016

* New function argument `midpoint="median"` uses the median to determine the midpoint for the ribbon. Previously, the midpoint was halfway between the minimum and maximum data values.  Use `midpoint=NULL` or `midpoint="midrange"` to restore the old behavior.


# desplot 1.0 - Dec 2015

* The `desplot` package has been split off from the `agridat` package.


# desplot 0.0 - 2008

* Original creation of function.
