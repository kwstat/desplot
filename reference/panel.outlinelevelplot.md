# Panel Function for desplot

This is a panel function for `desplot` which fills cells with a
background color and adds outlines around blocks of cells.

## Usage

``` r
panel.outlinelevelplot(
  x,
  y,
  z,
  subscripts,
  at,
  ...,
  alpha.regions = 1,
  out1f,
  out1g,
  out2f,
  out2g,
  dq
)
```

## Arguments

- x:

  Coordinates

- y:

  Coordinates

- z:

  Value for filling each cell.

- subscripts:

  For compatibility.

- at:

  Breakpoints for the colors.

- ...:

  Other

- alpha.regions:

  Transparency for fill colors. Not well tested.

- out1f:

  Factor to use for outlining (level 1).

- out1g:

  Factor to use for outlining (level 2).

- out2f:

  Graphics parameters to use for outlining.

- out2g:

  Graphics parameters to use for outlining.

- dq:

  Indicator of which cells should be flagged for data quality.

## Details

It does not add the text labels, numbers, or colors.

The rule for determining where to draw outlines is to compare the levels
of the factor used for outlining. If bordering cells have different
levels of the factor, then a border is drawn. 'NA' values are ignored
(otherwise, too many lines would be drawn).

The code works, but is probably overkill and has not been streamlined.

## References

None
