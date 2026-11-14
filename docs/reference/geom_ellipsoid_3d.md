# Ellipsoids

Draws one solid ellipsoid per row, centred on x, y, z. Give either the
semi-axes `rx`, `ry` and `rz`, turned by an optional `rotation` (a
list-column of 3 x 3 rotation matrices whose columns are the directions
of the three semi-axes), or `sigma`, a list-column of symmetric positive
semidefinite 3 x 3 shape matrices: the ellipsoid is then the set of
points p with (p - centre)' sigma^-1 (p - centre) \<= 1, as in rgl's
`ellipse3d()` at `t = 1`. For a covariance matrix and a confidence
level, scale sigma by `qchisq(level, 3)`. All lengths are in the
scale-transformed units of the position axes, and the ellipsoids expand
the position scales.

## Usage

``` r
geom_ellipsoid_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  detail = 2L,
  axes = FALSE,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- detail:

  Subdivision level of the icosphere, from 0 to 6.

- axes:

  Whether to draw the three principal axes of each ellipsoid as opaque
  segments through its centre, in the `colour` aesthetic or, where that
  is missing, the `fill` darkened by half. Inside an opaque ellipsoid
  they are hidden; give the ellipsoids an `alpha` below one to see them.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

Each ellipsoid is an icosphere of `20 * 4^detail` triangles stretched
onto the principal axes; in curvilinear frames it is placed at its
mapped centre and keeps its shape.

## See also

[`stat_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/stat_ellipsoid_3d.md)
estimates the ellipsoids from groups of points;
[`geom_sphere_3d()`](https://gillescolling.com/ggplot3d/reference/geom_sphere_3d.md),
[`geom_glyph_3d()`](https://gillescolling.com/ggplot3d/reference/geom_glyph_3d.md).

## Examples

``` r
d <- data.frame(x = c(0, 3), y = c(0, 1), z = c(0, 1), rx = c(1, 0.5),
                ry = c(0.5, 0.5), rz = c(0.3, 1.2))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, rx = rx, ry = ry, rz = rz)) +
  geom_ellipsoid_3d(fill = "steelblue") + coord_3d(aspect = "data")

groups <- split(iris[1:3], iris$Species)
e <- data.frame(t(sapply(groups, colMeans)), species = names(groups))
names(e)[1:3] <- c("x", "y", "z")
e$sigma <- lapply(groups, function(g) cov(g) * qchisq(0.95, 3))
ggplot2::ggplot(e, ggplot2::aes(x, y, z = z, sigma = sigma, fill = species)) +
  geom_ellipsoid_3d(alpha = 0.6, axes = TRUE) + coord_3d()
```
