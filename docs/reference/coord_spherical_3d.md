# Spherical three-dimensional coordinates

Position aesthetics set longitude, latitude and distance from the
centre. Lines and faces are subdivided so they follow the sphere; the
rear half of the outer shell carries the graticule of longitude and
latitude breaks.

## Usage

``` r
coord_spherical_3d(
  theta = "x",
  phi = "y",
  r = "z",
  start = 0,
  end = NULL,
  philim = NULL,
  inner.radius = 0,
  direction = 1,
  panes = "back",
  shells = c("outer", "breaks"),
  ...
)
```

## Arguments

- theta, phi, r:

  Position aesthetics, `"x"`, `"y"` or `"z"`, used for longitude,
  latitude and radius.

- start, end:

  Longitude range in radians, measured from 12 o'clock seen from above;
  `end = NULL` is one full turn.

- philim:

  Latitude range in radians, default from the south pole to the north
  pole.

- inner.radius:

  Fraction of the outer radius left empty at the centre.

- direction:

  1 for clockwise, -1 for anticlockwise.

- panes:

  Panes to draw: `"back"` (the rear half of the outer shell and of any
  cut surfaces), `"front"` (the parts of the other panes that face the
  viewer), or names among `"shell"`, `"core"`, `"south"`, `"north"`,
  `"start"`, `"end"`.

- shells:

  `"outer"` draws the graticule on the outer shell; `"breaks"` adds the
  rear half of a graticule shell, without fill, at each major break of
  the radius scale.

- ...:

  Other
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
  arguments: camera, limits, axes, box, light, backend.

## Value

A Coord3D ggproto object.

## Examples

``` r
g <- expand.grid(lon = seq(0, 330, by = 30), lat = seq(-60, 60, by = 30))
g$r <- 1 + 0.2 * cos(g$lat * pi / 90)
ggplot2::ggplot(g, ggplot2::aes(lon, lat, z = r)) +
  geom_point_3d() + coord_spherical_3d(elevation = 20)
```
