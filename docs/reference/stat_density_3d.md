# Trivariate density isosurfaces

Evaluates an axis-aligned Gaussian product kernel on transformed x/y/z
scale ranges. h contains Gaussian standard deviations in transformed
units. Automatic bandwidths use normal-reference marginal bandwidths
with sample size exponent -1/7. adjust multiplies automatic bandwidths.
Numeric levels are density thresholds; probs() specifies absolute
enclosed probability contents estimated by tensor trapezoidal
quadrature. Computed level and density contain the resolved density
threshold.

## Usage

``` r
stat_density_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  h = NULL,
  adjust = 1,
  n = 32L,
  levels = NULL,
  caps = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_isodensity_3d(...)
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

  Grid resolution, scalar or three integers.

- levels:

  Numeric scalar-field thresholds; NULL uses pretty levels.

- caps:

  Whether to cap the sublevel region on the box faces.

## Examples

``` r
set.seed(1)
d <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_isodensity_3d(h = 0.6, levels = 0.02, n = 12) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_density_3d(h = 0.6, levels = 0.02, n = 8) + coord_3d()
```
