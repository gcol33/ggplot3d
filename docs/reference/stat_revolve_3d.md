# Surfaces of revolution

Turns each group's profile, given by the `r` aesthetic (distance from
the axis) and the aesthetic of the axis (`z` for `axis = "z"`), about a
line parallel to that axis through `centre`, in transformed position
units. The profile rows are joined in order; `closed = TRUE` joins the
last to the first (a torus from a circle). `n` angles cover `angle`
degrees; a full turn wraps without a seam, and profile points at zero
`r` become single poles. Normals point outwards where the profile runs
towards increasing axis values. Numeric aesthetics are averaged over
each face.

## Usage

``` r
stat_revolve_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  axis = c("z", "x", "y"),
  n = 48L,
  closed = FALSE,
  angle = c(0, 360),
  centre = c(0, 0),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_revolve_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- axis:

  Axis of revolution: `"z"` (r in the x-y plane), `"x"` or `"y"`.

- n:

  Number of angles.

- closed:

  Whether the profile is a closed loop.

- angle:

  Range of the revolution in degrees.

- centre:

  Position of the axis in the other two coordinates, in the cyclic order
  (x, y), (y, z) or (z, x).

## Value

A ggplot2 layer.

## Examples

``` r
vase <- data.frame(h = seq(0, 2, length.out = 30))
vase$r <- 0.6 + 0.3 * sin(2.5 * vase$h) - 0.15 * vase$h
ggplot2::ggplot(vase, ggplot2::aes(r = r, z = h, fill = h)) +
  geom_revolve_3d(n = 36) + coord_3d()
```
