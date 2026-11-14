# Smooth curves through points

Replaces each group's points, in row order, by `n` samples of a smooth
curve. `"catmull-rom"` interpolates the points with the centripetal
Catmull-Rom spline (knot spacing the square root of the chord length),
which has no cusps or self-intersections within a segment; `"natural"`
interpolates with a natural (or, when closed, periodic) cubic spline in
chord length; `"bspline"` approximates the points with a uniform cubic
B-spline that starts and ends on the end points. Other numeric
aesthetics are interpolated linearly between the points; other
aesthetics keep the value of the preceding point. A closed curve repeats
its first sample at the end so that a path closes.

## Usage

``` r
stat_spline_3d(
  mapping = NULL,
  data = NULL,
  geom = ggplot2::GeomPath,
  position = "identity",
  ...,
  n = 100L,
  type = c("catmull-rom", "bspline", "natural"),
  closed = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom used to draw the curve.

- n:

  Number of samples.

- type:

  `"catmull-rom"`, `"bspline"` or `"natural"`.

- closed:

  Whether the curve is a closed loop.

## Value

A ggplot2 layer.

## References

Yuksel, Schaefer and Keyser (2011). Parameterization and applications of
Catmull-Rom curves. Computer-Aided Design 43, 747-755.
[doi:10.1016/j.cad.2010.08.008](https://doi.org/10.1016/j.cad.2010.08.008)

## Examples

``` r
d <- data.frame(x = c(0, 1, 2, 3, 4), y = c(0, 1, 0, 1, 0), z = c(0, 1, 0, -1, 0))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d() + stat_spline_3d(type = "natural") + coord_3d()
```
