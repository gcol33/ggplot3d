# Sample a bivariate function in three dimensions

The sampling grid is evenly spaced in transformed x and y coordinates.
The function receives x and y in original data units and returns z in
data units. The z scale transforms the returned heights once. Newly
mapped after_stat variables follow ggplot2 scale transformation.

## Usage

``` r
stat_function_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  fun,
  n = 40L,
  args = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_function_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- fun:

  Vectorised function of x and y.

- n:

  Grid resolution, a scalar or two integers.

- args:

  Additional named arguments passed to fun.

## Value

A ggplot2 layer.

## Examples

``` r
ggplot2::ggplot() +
  stat_function_3d(fun = function(x, y) sin(x) * cos(y), n = 15) +
  ggplot2::xlim(-3, 3) + ggplot2::ylim(-3, 3) + coord_3d()
ggplot2::ggplot() +
  geom_function_3d(fun = function(x, y) x^2 + y^2, n = 8) +
  ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + coord_3d()
```
