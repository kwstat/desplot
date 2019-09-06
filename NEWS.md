

Idea: For this warning, maybe I should return a tibble
with the multiple data???

In desplot(rep ~ y * x | loc, data = dd) :
  There are multiple data for each x/y/panel combination

Idea: Add a lightweight 'x' for bad-quality data.  How to make this general (for multiple quality levels)?

# desplot 1.5 - Apr 2019

* Beta version of `desplot()` to create `ggplot2` graphics.

* New argument `subset` to subset data before analysis.

# desplot 1.3 - Oct 2017

* Bug fix.

# desplot 1.2 - Jul 2017

* Now using `testthat` and `covr` packages. (Currently `covr` fails).

# desplot 1.1 - Dec 2016

* A new function argument `midpoint="median"` now uses the median to determine the midpoint for the ribbon. Previously, the midpoint was halfway between the minimum and maximum data values.  Use `midpoint=NULL` or `midpoint="midrange"` to restore the old behavior.

* Minor fix so vignette example with `yates.oats` data will work with next version of `agridat`.

# desplot 1.0 - Dec 2015

* The `desplot` package has been split off from the `agridat` package.

# desplot 0.0 - 2008

* Original creation of function.

