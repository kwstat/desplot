# NA

## Summary

This PR adds support for the `aspect` parameter in
[`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md) to
control cell aspect ratios for true-scale field maps.

## Changes

- Added `aspect` parameter to
  [`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md)
  function signature (line 81 in `R/ggdesplot.R`)
- Uses `coord_fixed(ratio=aspect, expand=FALSE)` when aspect is
  specified
- When `aspect` is `NULL`, uses `coord_cartesian(expand=FALSE)` as
  before (default behavior unchanged)
- Updated
  [`desplot()`](http://kwstat.github.io/desplot/reference/desplot.md) to
  pass `...` arguments through to
  [`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md)
  (line 330 in `R/desplot.R`)

## Reprex

``` r
library(desplot)
library(agridat)

# Example 1: aspect=1 creates square cells
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=1,
          main="aspect=1: Square cells")
```

![](https://i.imgur.com/example1.png)

``` r
# Example 2: aspect=2 creates cells 2x taller than wide
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=2,
          main="aspect=2: Cells 2x taller than wide")
```

![](https://i.imgur.com/example2.png)

``` r
# Example 3: Using physical field dimensions (from README examples)
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=511/176,
          main="aspect=511/176: True-scale field map")
```

![](https://i.imgur.com/example3.png)

``` r
# Example 4: With facets (besag.met dataset)
ggdesplot(besag.met, yield ~ col*row|county,
          out1=rep, out2=block,
          aspect=1.0,
          main="aspect=1.0 with facets")
```

![](https://i.imgur.com/example4.png)

## Testing

Tested with various aspect ratios: - `aspect=1` → Cells are square ✓ -
`aspect=2` → Cells are 2x taller than wide ✓ - `aspect=0.5` → Cells are
2x wider than tall ✓ - `aspect=511/176` → Matches physical field
dimensions ✓

The implementation uses `coord_fixed(ratio=aspect)` which directly
controls the ratio of y-axis units to x-axis units.

## Note on Lattice vs ggplot2 Behavior

During testing, I observed that cell shapes in
[`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md)
with `aspect=1` create square cells (as expected), while the lattice
version
[`desplot()`](http://kwstat.github.io/desplot/reference/desplot.md) with
`aspect=1` produces cells that are not square. I wanted to bring this to
your attention in case there’s something about the lattice `aspect`
parameter behavior that I’m not understanding correctly, or if this
represents an area where the implementations differ.

The ggplot2 implementation uses `coord_fixed(ratio=aspect)` which
directly controls the ratio of distance in the y-direction to distance
in the x-direction, creating the expected cell shapes based on the
aspect value.

## Benefits

- Adds documented feature to ggplot2 version
- Cells correctly scale according to aspect ratio value
- Enables true-scale field mapping with
  [`ggdesplot()`](http://kwstat.github.io/desplot/reference/desplot.md)
- Brings ggplot2 version closer to feature parity with lattice version

## Related

This is part of a series of focused improvements to ggdesplot: - PR
\#13: strip.cex parameter fix - PR \#14: out1.gpar/out2.gpar parameter
fix - PR \#15: This aspect ratio feature

Thank you for maintaining this excellent package!
