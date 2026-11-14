# Quaternary simplex coordinates

Four nonnegative parts `a`, `b`, `c` and `d` are closed to proportions
and placed by barycentric position in a regular tetrahedron. Each face
carries isolines of its three parts; each vertex carries the title of
its part.

## Usage

``` r
coord_simplex_3d(axes = c("a", "b", "c", "d"), panes = c("back", "floor"), ...)
```

## Arguments

- axes:

  Guides to draw, any of `"a"`, `"b"`, `"c"` and `"d"`.

- panes:

  Panes to draw: `"back"` (rear faces other than the base), `"floor"`
  (the base `"abc"`), `"front"` (the other faces facing the viewer), or
  face names `"abc"`, `"bcd"`, `"acd"`, `"abd"`.

- ...:

  Other
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
  arguments: camera, box, light, backend.

## Value

A Coord3D ggproto object.

## Examples

``` r
set.seed(1)
w <- matrix(stats::rexp(80), ncol = 4)
d <- data.frame(a = w[, 1], b = w[, 2], c = w[, 3], d = w[, 4])
ggplot2::ggplot(d, ggplot2::aes(a = a, b = b, c = c, d = d)) +
  geom_point_3d() + coord_simplex_3d()
```
