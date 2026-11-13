# Level surfaces of a trivariate function

Evaluates fun on transformed x/y/z scale ranges and passes the scalar
field to the same tetrahedral extraction used by stat_isosurface_3d.

## Usage

``` r
stat_isofunction_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  fun,
  n = 24L,
  args = list(),
  levels = NULL,
  caps = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_isofunction_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- fun:

  Vectorised scalar function of x, y and z.

- n:

  Grid resolution, scalar or three integers.

- args:

  Additional named arguments passed to fun.

- levels:

  Numeric scalar-field thresholds; NULL uses pretty levels.

- caps:

  Whether to cap the sublevel region on the box faces.

## Examples

``` r
ggplot2::ggplot() +
  geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
                     n = 10, levels = 0.5) + coord_3d()
ggplot2::ggplot() +
  stat_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
                     n = 8, levels = 0.5) + coord_3d()
```
