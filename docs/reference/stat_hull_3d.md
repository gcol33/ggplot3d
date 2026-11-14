# Convex hulls in three dimensions

Native Quickhull assigns outside points to facets, selects the furthest
point of an outside set, replaces visible facets by the horizon cone,
and repartitions their outside sets. Duplicate points are removed.
Coplanar inputs return a triangulated planar convex polygon; collinear
inputs return no faces. Orientation uses a fixed interior point and
scale-relative floating-point tolerance. x/y/z positions are already
transformed.

## Usage

``` r
stat_hull_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  space = "data",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_hull_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- space:

  `"data"` takes the hull of the positions. `"ilr"` takes the hull of a
  composition mapped with `a`, `b`, `c` (and `d`) in isometric log-ratio
  coordinates and keeps its facets on the original points.

## Details

The hull faces keep the rows of their corner points, so a fill given per
point, such as `fill = I(hex)`, reaches every vertex. With
`shading = "smooth"` each face interpolates its corner colours in linear
light, and with `shade = FALSE` the vertices show their colours exactly.
A discrete fill splits the points into groups, so `group = 1` keeps one
hull.

## References

Barber, Dobkin and Huhdanpaa (1996). The Quickhull Algorithm for Convex
Hulls. ACM TOMS 22, 469-483.
[doi:10.1145/235815.235821](https://doi.org/10.1145/235815.235821)

## Examples

``` r
d <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_hull_3d(fill = "steelblue") + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_hull_3d(fill = "steelblue") + coord_3d()
# Colours per vertex, interpolated over the faces.
d$hex <- grDevices::rgb(d$x, d$y, d$z)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_hull_3d(ggplot2::aes(fill = I(hex), group = 1), shading = "smooth", shade = FALSE) +
  coord_3d()
```
