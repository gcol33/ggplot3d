# Contours following a triangulated surface or a horizontal plane

Contours following a triangulated surface or a horizontal plane

## Usage

``` r
geom_contour_3d(
  mapping = NULL,
  data = NULL,
  stat = StatContour3D,
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  plane = c("surface", "floor"),
  method = c("grid", "delaunay"),
  max_edge = Inf,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_contour_filled_3d(
  mapping = NULL,
  data = NULL,
  stat = StatContourFilled3D,
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  plane = c("surface", "floor"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- bins, binwidth, breaks:

  Level controls in transformed z units. Explicit breaks take
  precedence.

- plane:

  Either surface, floor, or a numeric z in original units. A flat plane
  is a shorthand for `position = position_project_3d("zmin")` or
  `position_project_3d(list(axis = "z", at = plane))`; use
  [`position_project_3d()`](https://gillescolling.com/ggplot3d/reference/position_project_3d.md)
  directly to draw contours on a side wall.

- method:

  `"grid"` triangulates a complete x/y grid as geom_surface_3d() does;
  `"delaunay"` triangulates scattered x/y positions. Mesh input
  ([`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md))
  uses its own faces.

- max_edge:

  Longest x/y edge kept in a Delaunay triangulation.

## Value

A ggplot2 layer.

## Details

Contour lines are traced by marching triangles on the triangles of the
surface: the field is linear on each triangle, crossings on a shared
edge are shared, and segments are chained into open lines and closed
loops. The field is z, or the `value` aesthetic when mapped, so any
scalar on any triangulated surface can be contoured: a variable measured
on a terrain, or `value = n . p` for sections of the surface by parallel
planes. Mesh input (a `mesh3d` or
[`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
data with `.face`, `.corner` and `.vertex` keys) is contoured on its own
faces, polygons fanned from their first corner. Filled contours follow z
only; isoband operates on the same grid triangles used by
geom_surface_3d(). Default levels are interior pretty(range, n = 10)
values. Surface bands are lifted by barycentric interpolation within
each triangle. Floor contours use the lower expanded panel boundary and
are painted with the floor pane. The source height range trains the z
scale for either projection.

## Examples

``` r
d <- expand.grid(x = seq(-2, 2, length.out = 12), y = seq(-2, 2, length.out = 12))
d$z <- exp(-d$x^2 - d$y^2)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d() + geom_contour_3d(bins = 4, colour = "navy") + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d() +
  geom_contour_3d(ggplot2::aes(value = x + y), bins = 6, colour = "navy") + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_contour_filled_3d(bins = 4, plane = "floor") + coord_3d()
```
