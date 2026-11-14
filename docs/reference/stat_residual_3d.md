# Residuals from a fitted surface

Fits a model of z on x and y as
[`stat_smooth_3d()`](https://gillescolling.com/ggplot3d/reference/stat_smooth_3d.md)
does and draws a vertical segment from each observation to the fitted
surface, the 3D counterpart of residual drop lines in regression
diagnostics. `method`, `formula`, `span`, `method.args`, `view` and
`cond` work as in
[`stat_smooth_3d()`](https://gillescolling.com/ggplot3d/reference/stat_smooth_3d.md),
so pairing both layers with the same arguments draws the residuals of
the surface that is shown.

## Usage

``` r
stat_residual_3d(
  mapping = NULL,
  data = NULL,
  geom = ggplot2::GeomSegment,
  position = "identity",
  ...,
  method = "loess",
  formula = NULL,
  span = 0.75,
  method.args = list(),
  view = NULL,
  cond = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_residual_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom drawing the residuals; segments by default.

- method:

  A fitting function, the name of one (`"loess"`, `"lm"`, `"gam"`,
  `"glm"`, ...), or a fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method. `"gam"`
  requires mgcv.

- formula:

  Model formula in x, y and z, for a fitting function.

- span:

  Loess span.

- method.args:

  Named arguments passed to the fitting function.

- view:

  For a fitted model, the names of its predictors drawn on x and y.

- cond:

  For a fitted model, a named list holding a value for each other
  predictor.

## Computed variables

- fitted:

  Fitted height at the observation, in transformed z.

- resid:

  Observed minus fitted height, in transformed z.

- sign:

  Sign of the residual: -1, 0 or 1.

## Examples

``` r
set.seed(1)
d <- data.frame(x = runif(40), y = runif(40))
d$z <- with(d, x + 2 * y + rnorm(40, sd = 0.2))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_smooth_3d(method = "lm", se = FALSE, n = 6, alpha = 0.6) +
  geom_residual_3d(ggplot2::aes(colour = ggplot2::after_stat(factor(sign))), method = "lm") +
  geom_point_3d() + coord_3d()
```
