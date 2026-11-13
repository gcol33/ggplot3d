# Grouped one-dimensional density ridges

Each group has a single position on the perpendicular x/y axis.
Densities are fitted along the chosen transformed axis. base and height
specify the baseline and density multiplier in transformed z units. Each
ridge is a filled, triangulated vertical sheet. Use group for multiple
ridges. Density tails within floating-point roundoff of zero are set to
zero; collapsed triangles are omitted.

## Usage

``` r
stat_ridgeline_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  along = "x",
  n = 128L,
  bw = "nrd0",
  adjust = 1,
  base = 0,
  height = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_ridgeline_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- along:

  Axis along which observations vary, x or y.

- n:

  Grid resolution, a scalar or two integers.

- bw:

  Bandwidth passed to stats::density.

- adjust:

  Positive automatic bandwidth multiplier, scalar or one per axis.

- base:

  Baseline in transformed z units.

- height:

  Density height multiplier in transformed z units.

## Examples

``` r
ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, as.numeric(Species),
  group = Species, fill = Species)) + geom_ridgeline_3d(n = 32) + coord_3d()
ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, as.numeric(Species),
  group = Species, fill = Species)) + stat_ridgeline_3d(n = 24) + coord_3d()
```
