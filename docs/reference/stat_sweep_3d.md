# Sweep a profile along a curve

Sweeps a two-dimensional `profile` along each group's path as
[`geom_tube_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md)
does with a circle: profile point (a, b) goes to
`p + radius * (a * r + b * s)`, where r and s are the
rotation-minimising normal and binormal at path point p, after turning
by the `twist` aesthetic. A profile of three or more points is a closed
polygon (a repeated last point is dropped), wound counterclockwise so
that normals point outwards; two points, or `profile_closed = FALSE`,
give an open strip. `radius` (aesthetic or parameter, default 1) scales
the profile, in millimetres on the page with `radius.unit = "mm"`.

## Usage

``` r
stat_sweep_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomSweep3D,
  position = "identity",
  ...,
  profile,
  radius = 1,
  radius.unit = "data",
  cap = c("round", "flat", "none"),
  normal = NULL,
  closed = FALSE,
  profile_closed = NULL,
  spline = NULL,
  n = 100L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_sweep_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom used to draw the surface.

- profile:

  Two-column matrix or data frame of profile points.

- radius:

  Scale factor for the profile, also available as an aesthetic.

- radius.unit:

  Unit of the cross-section size: `"data"` (transformed position units)
  or `"mm"` (millimetres on the page).

- cap:

  End caps of a tube on an open curve: `"round"`, `"flat"` or `"none"`.

- normal:

  Direction the first cross section is oriented towards: the ribbon's
  surface normal, or the profile's first axis in stat_sweep_3d().

- closed:

  Whether the path is a closed loop.

- profile_closed:

  Whether the profile is a closed polygon; NULL treats profiles of three
  or more points as closed.

- spline:

  NULL, or a
  [`stat_spline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_spline_3d.md)
  type to smooth the path first.

- n:

  Number of spline samples.

## Value

A ggplot2 layer.

## Examples

``` r
star <- t(sapply(0:9, function(k) (1 + (k %% 2)) / 2 * c(cos(k * pi / 5), sin(k * pi / 5))))
path <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 0, 1), z = c(0, 0.5, 1, 1.5))
ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) +
  stat_sweep_3d(profile = star * 0.3, spline = "natural") + coord_3d()
```
