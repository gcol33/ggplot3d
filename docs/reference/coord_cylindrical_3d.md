# Cylindrical three-dimensional coordinates

One position aesthetic sets the angle around a vertical axis, another
the distance from that axis, and the third the height. Lines and faces
that are straight in the data are subdivided so they follow the
cylinder, so paths become arcs and bars become ring sectors with exact
visibility.

## Usage

``` r
coord_cylindrical_3d(
  theta = "x",
  r = "y",
  start = 0,
  end = NULL,
  inner.radius = 0,
  direction = 1,
  panes = c("back", "floor"),
  ...
)
```

## Arguments

- theta, r:

  Position aesthetics, `"x"`, `"y"` or `"z"`, used for the angle and the
  radius. The remaining one is the height.

- start:

  Angle in radians, measured from 12 o'clock seen from above, where the
  angle axis starts.

- end:

  Angle in radians where the angle axis ends; `NULL` is one full turn
  after `start`.

- inner.radius:

  Fraction of the outer radius left empty at the centre.

- direction:

  1 for clockwise, -1 for anticlockwise.

- panes:

  Panes to draw: `"back"` (the rear half of the outer wall, of the inner
  wall and of the cut planes of a partial turn), `"floor"`, `"front"`
  (the parts of the other panes that face the viewer, see
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)),
  or names among `"floor"`, `"top"`, `"wall"`, `"inner"`, `"start"`,
  `"end"`.

- ...:

  Other
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
  arguments: camera, limits, axes, box, light, backend.

## Value

A Coord3D ggproto object.

## Examples

``` r
d <- data.frame(month = 1:12, site = rep(c(1, 2), 6), value = c(3:8, 8:3))
ggplot2::ggplot(d, ggplot2::aes(month, site, z = value)) +
  geom_point_3d(size = 2) + coord_cylindrical_3d(elevation = 35)
```
