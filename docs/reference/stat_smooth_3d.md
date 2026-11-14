# Fit a bivariate response surface

Fits a model of z on x and y in each group and draws its predictions on
an x/y grid. As in
[`ggplot2::stat_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html),
`method` is a fitting function or its name, called as
`method(formula, data = data, weights = weight, ...)` with `method.args`
in `...`, and the surface is `predict(fit, newdata, se.fit = )`. Any
model with such a [`predict()`](https://rdrr.io/r/stats/predict.html)
method works: thin-plate or tensor-product smooths from mgcv, robust or
generalised linear models, random forests. Fitting functions see
transformed x, y and z. The default formula is `z ~ x + y`; `"gam"` uses
`z ~ s(x, y)` from mgcv, and loess takes `span`.

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
  domain = c("box", "hull"),
  view = NULL,
  cond = list(),
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

  A fitting function, the name of one (`"loess"`, `"lm"`, `"gam"`,
  `"glm"`, ...), or a fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method. `"gam"`
  requires mgcv.

- formula:

  Model formula in x, y and z, for a fitting function.

- n:

  Grid resolution, a scalar or two integers.

- se:

  Confidence display: `"surfaces"` (or `TRUE`) draws translucent lower
  and upper surfaces; `"band"` closes them with walls along the edge of
  the domain into one translucent volume around the fit; `"fill"` draws
  the fit alone, filled by the standard error unless fill is mapped or
  set; `"none"` (or `FALSE`) draws the fit alone.

- level:

  Pointwise confidence level.

- span:

  Loess span.

- method.args:

  Named arguments passed to the fitting function.

- se.alpha:

  Opacity multiplier for confidence surfaces and walls.

- domain:

  `"box"` fills the x/y range of the panel; `"hull"` clips the surface
  to the convex hull of the group's observations, so the model is not
  drawn where it extrapolates. Grid cells crossed by the hull edge are
  cut along it and the model is evaluated on the cut.

- view:

  For a fitted model, the names of its predictors drawn on x and y.

- cond:

  For a fitted model, a named list holding a value for each other
  predictor.

## Details

`method` may instead be a model fitted beforehand. It is evaluated in
data units: the grid's x and y are back-transformed and named by `view`,
the predictions are transformed by the z scale. When `view` is `NULL` it
takes the variable names of the layer's x and y mappings, if these are
bare column names, else `c("x", "y")`. Other predictors of the model
take the values in `cond`, or by default the observed value closest to
the median and the most frequent level, as in
[`mgcv::vis.gam()`](https://rdrr.io/pkg/mgcv/man/vis.gam.html).

Bounds are pointwise confidence intervals for the conditional mean, with
Student t quantiles on the residual degrees of freedom, and normal
quantiles on the link scale mapped through the inverse link for glm
fits, as in ggplot2.

## Computed variables

- surface:

  `"fit"`, `"lower"`, `"upper"`, and `"side"` for the walls of a band.

- se:

  Standard error of the fitted mean on the model's scale (link scale for
  glm fits), averaged over each triangle.

- z_mean:

  Transformed mean height of each triangle.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
d$z <- with(d, x + y + sin(3*x*y) / 5)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_smooth_3d(method = "lm", n = 12) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_smooth_3d(method = "lm", se = FALSE, n = 8) + coord_3d()

# A fitting function with its own formula: a quadratic response surface
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_smooth_3d(method = lm, formula = z ~ poly(x, y, degree = 2),
    se = "band", n = 12) + coord_3d()

# A model fitted beforehand, drawn over the hull of the data and filled by
# its standard error
fit <- lm(mpg ~ poly(wt, 2) + hp, data = mtcars)
ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
  geom_smooth_3d(method = fit, domain = "hull", se = "fill", n = 20) +
  geom_point_3d() + coord_3d()
```
