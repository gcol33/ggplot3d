# Hexagonal bins drawn as prisms

`geom_hex_3d()` draws one upright hexagonal prism per row, centred on x
and y and extending from the baseline (or `zmin`) to `z`. By default it
counts observations in hexagonal bins with `stat_bin_hex_3d()`, the
three-dimensional form of
[`ggplot2::geom_hex()`](https://ggplot2.tidyverse.org/reference/geom_hex.html)
and of the extruded hexagon layers of deck.gl. `stat_summary_hex_3d()`
sets each prism's height to a summary of the mapped `z` values in the
hexagon.

## Usage

``` r
geom_hex_3d(
  mapping = NULL,
  data = NULL,
  stat = StatBinHex3D,
  position = "identity",
  ...,
  width = NULL,
  depth = NULL,
  base = 0,
  cap = "both",
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_bin_hex_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomHex3D,
  position = "identity",
  ...,
  bins = 30L,
  binwidth = NULL,
  drop = TRUE,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_summary_hex_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomHex3D,
  position = "identity",
  ...,
  fun = "mean",
  fun.args = list(),
  bins = 30L,
  binwidth = NULL,
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

- stat:

  Stat that computes the bins.

- width, depth:

  Hexagon size along x and y; see Details.

- base:

  Baseline of the columns in original z units.

- cap:

  Caps to draw: both, top, bottom or none.

- shade:

  Whether to apply lighting.

- geom:

  Geom used to draw the bins.

- bins:

  Number of bins along x and y.

- binwidth:

  Bin widths along x and y; overrides `bins`.

- drop:

  For counts, remove empty bins; for summaries, remove bins whose
  summary is `NA`.

- fun:

  Summary function, or its name, applied to the `z` values of each bin;
  it must return one number.

- fun.args:

  List of further arguments to `fun`.

## Value

A ggplot2 layer.

## Details

Hexagons have pointed tops. Centres lie `binwidth[1]` apart along a row
and rows lie `binwidth[2] * sqrt(3) / 2` apart, every second row shifted
by half a hexagon, so the cells are regular when x and y are drawn at
the scale of their bin widths; the lattice is anchored at multiples of
the bin widths, so groups and panels share it. The default `bins = 30`
sets the bin widths to a thirtieth of the x and y ranges, as in ggplot2.
Each observation goes to the hexagon whose centre is nearest in units of
the bin widths.

With `stat = "identity"`, the hexagon size comes from `width` and
`depth` (columns or arguments): `width` is the distance between the flat
sides, along x, and `depth` the row spacing divided by `sqrt(3) / 2`,
along y. Without them the size follows from the spacing of the centres.

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

## References

Carr, D. B., Littlefield, R. J., Nicholson, W. L. and Littlefield, J. S.
(1987). Scatterplot matrix techniques for large N. Journal of the
American Statistical Association 82, 424-436.

## See also

[`stat_bin_2d_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_2d_3d.md)
for rectangular bins.

## Examples

``` r
p <- ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting))
p + geom_hex_3d(bins = 12) + coord_3d()
p + geom_hex_3d(ggplot2::aes(fill = ggplot2::after_stat(count)),
  binwidth = c(0.4, 5), colour = "grey20") + coord_3d()
set.seed(1)
d <- data.frame(x = rnorm(500), y = rnorm(500))
d$z <- d$x^2 + d$y^2
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_summary_hex_3d(bins = 10, fill = "darkorange") + coord_3d()
```
