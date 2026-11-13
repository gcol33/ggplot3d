# Fit a bivariate response surface

Fits and predictions use transformed x, y and z coordinates. The default
lm formula is z ~ x + y, loess uses a quadratic local surface, and gam
uses z ~ s(x, y). Bounds are pointwise confidence surfaces for the
conditional mean. The computed surface variable identifies fit, lower
and upper surfaces. The computed z_mean is the transformed mean height
of each triangle.

## Usage

``` r
stat_smooth_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  method = "loess",
  formula = NULL,
  n = 30L,
  se = TRUE,
  level = 0.95,
  span = 0.75,
  method.args = list(),
  se.alpha = 0.2,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_smooth_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- method:

  One of loess, lm or gam. gam requires mgcv.

- formula:

  Model formula using x, y and z.

- n:

  Grid resolution, a scalar or two integers.

- se:

  Whether to include lower and upper standard-error surfaces.

- level:

  Pointwise confidence level.

- span:

  Loess span.

- method.args:

  Named arguments passed to the model fitter.

- se.alpha:

  Opacity multiplier for confidence surfaces.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
d$z <- with(d, x + y + sin(3*x*y) / 5)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_smooth_3d(method = "lm", n = 12) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_smooth_3d(method = "lm", se = FALSE, n = 8) + coord_3d()
```
