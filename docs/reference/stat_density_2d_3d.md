# Bivariate kernel density as a height surface

Uses an axis-aligned Gaussian product kernel on transformed x/y
coordinates. Bandwidth h follows the MASS kde2d convention used by
ggplot2: the Gaussian standard deviation is h / 4. Automatic bandwidths
are 4 \* bw.nrd per axis, with bw.nrd0 for a zero interquartile range.
adjust applies to automatic h. Height is density in original z units and
receives the z transformation once. Computed density, ndensity and count
are averaged per triangle for fill mapping.

## Usage

``` r
stat_density_2d_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  h = NULL,
  adjust = 1,
  n = 40L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- h:

  Positive bandwidth, scalar or one per axis; NULL estimates it.

- adjust:

  Positive automatic bandwidth multiplier, scalar or one per axis.

- n:

  Grid resolution, a scalar or two integers.

## Examples

``` r
ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) +
  stat_density_2d_3d(n = 20) + coord_3d()
```
