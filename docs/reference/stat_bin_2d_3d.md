# Two-dimensional bins drawn as columns

`stat_bin_2d_3d()` divides the x/y plane into rectangles and counts the
observations, optionally weighted, in each; the count becomes the height
of a column. `geom_histogram_3d()` is the same layer under the bar-chart
name, the three-dimensional form of a bivariate histogram.
`stat_summary_2d_3d()` bins the same way and sets each column's height
to a summary of the mapped `z` values in the cell, for example their
mean.

## Usage

``` r
stat_bin_2d_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomCol3D,
  position = "identity",
  ...,
  bins = 30L,
  binwidth = NULL,
  breaks = NULL,
  boundary = NULL,
  drop = TRUE,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_histogram_3d(
  mapping = NULL,
  data = NULL,
  stat = StatBin2d3D,
  position = "identity",
  ...,
  bins = 30L,
  binwidth = NULL,
  breaks = NULL,
  boundary = NULL,
  drop = TRUE,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_summary_2d_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomCol3D,
  position = "identity",
  ...,
  fun = "mean",
  fun.args = list(),
  bins = 30L,
  binwidth = NULL,
  breaks = NULL,
  boundary = NULL,
  drop = TRUE,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom used to draw the bins.

- bins:

  Number of bins along x and y.

- binwidth:

  Bin widths along x and y; overrides `bins`.

- breaks:

  Bin edges; overrides `bins` and `binwidth`.

- boundary:

  Position of one bin edge when `binwidth` is given.

- drop:

  For counts, remove empty bins; for summaries, remove bins whose
  summary is `NA`.

- base:

  Baseline of the columns in original z units.

- shade:

  Whether to apply lighting.

- stat:

  Stat that computes the bins.

- fun:

  Summary function, or its name, applied to the `z` values of each bin;
  it must return one number.

- fun.args:

  List of further arguments to `fun`.

## Value

A ggplot2 layer.

## Details

Bins span the trained x and y scale ranges. With `bins` the range of
each axis is divided into that many equal intervals. With `binwidth` the
edges fall on `boundary + k * binwidth` (by default on multiples of
`binwidth`), so bins line up across layers and panels. `breaks` gives
the edges directly; observations outside them are not counted. Every
argument takes one value for both axes or one per axis (`breaks`: a
vector, or a list of two). Columns touch, as histogram bars do; groups
share the same bins, so
[`position_stack_3d()`](https://gillescolling.com/ggplot3d/reference/position_stack_3d.md)
stacks them.

## Computed variables

- `count`:

  Number of observations in the bin (sum of weights).

- `density`:

  Count divided by the total count and by the bin area, so the column
  volumes sum to one.

- `ncount`, `ndensity`:

  Count and density divided by their maximum.

- `width`, `depth`:

  Bin size along x and y.

- `value`:

  Summaries only: the result of `fun`, also used as `z`.

## See also

[`stat_bin_hex_3d()`](https://gillescolling.com/ggplot3d/reference/geom_hex_3d.md)
for hexagonal bins,
[`stat_count_3d()`](https://gillescolling.com/ggplot3d/reference/geom_col_3d.md)
for counts at discrete x/y combinations,
[`stat_bin_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_3d.md)
for counts in boxes.

## Examples

``` r
p <- ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting))
p + geom_histogram_3d(binwidth = c(0.5, 5)) + coord_3d()
p + stat_bin_2d_3d(ggplot2::aes(fill = ggplot2::after_stat(density)), bins = 12) +
  coord_3d()
set.seed(1)
d <- data.frame(x = runif(400), y = runif(400))
d$z <- sin(3 * d$x) + d$y + rnorm(400, sd = 0.1)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_summary_2d_3d(fun = "median", bins = 8, fill = "steelblue") + coord_3d()
```
