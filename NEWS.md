
# desplot 1.1 - Dec 2016

A new function argument `midpoint="median"` now uses the median to determine the midpoint for the ribbon. Previously, the midpoint was halfway between the minimum and maximum data values.  Use `midpoint=NULL` to restore the old behavior.

Moved tests to tests/desplot_tests.R

Minor fix so vignette example with `yates.oats` data will work with next version of `agridat`.

# desplot 1.0 - Dec 2015

The `desplot` package has been split off from the `agridat` package.

# desplot 0.0 - 2008

Original creation of function.

