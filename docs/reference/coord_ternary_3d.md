# Ternary prism coordinates

Three nonnegative parts `a`, `b` and `c` are closed to proportions and
placed by barycentric position on an equilateral triangle; `z` stands on
it as a height, so a three-part composition carries a response. Parts
are mapped with the aesthetics `a`, `b` and `c` in place of `x` and `y`.
The map is affine, so visibility stays exact.

## Usage

``` r
coord_ternary_3d(axes = c("a", "b", "c", "z"), panes = c("back", "floor"), ...)
```

## Arguments

- axes:

  Guides to draw, any of `"a"`, `"b"`, `"c"` and `"z"`.

- panes:

  Panes to draw: `"back"` (the rear walls), `"floor"`, `"front"` (the
  other panes facing the viewer), or names among `"floor"`, `"top"`,
  `"ab"`, `"bc"`, `"ca"`.

- ...:

  Other
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
  arguments: camera, z limits, box, light, backend.

## Value

A Coord3D ggproto object.

## Details

The part guides share the break set of the `x` scale, and their titles
come from the `a`, `b` and `c` mappings. Each part's ticks run along the
triangle edge where the next part is zero, and its title sits at the
vertex where the part is one.

## Examples

``` r
soil <- data.frame(sand = c(60, 20, 30, 10), silt = c(30, 50, 20, 30),
  clay = c(10, 30, 50, 60), yield = c(2, 5, 4, 3))
ggplot2::ggplot(soil, ggplot2::aes(a = sand, b = silt, c = clay, z = yield)) +
  geom_point_3d(size = 3) + coord_ternary_3d()
```
