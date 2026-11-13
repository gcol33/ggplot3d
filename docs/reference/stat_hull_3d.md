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

## References

Barber, Dobkin and Huhdanpaa (1996). The Quickhull Algorithm for Convex
Hulls. ACM TOMS 22, 469-483. <https://doi.org/10.1145/235815.235821>

## Examples

``` r
d <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_hull_3d(fill = "steelblue") + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_hull_3d(fill = "steelblue") + coord_3d()
```
