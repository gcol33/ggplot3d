# Project a layer onto the walls of the panel box

Draws a layer flattened onto panes of the panel box: shadows of points
on the floor, contours on a side wall, or a whole two-dimensional
ggplot2 layer such as a histogram or density drawn as a marginal on a
wall.

## Usage

``` r
position_project_3d(onto = "floor", axes = NULL)
```

## Arguments

- onto:

  Where the layer is drawn. `"floor"` is the low face of the vertical
  axis; `"back"` the rear one of each pair of vertical faces, chosen for
  the camera as for the panes drawn by
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md);
  a pane name of the panel frame (`"xmin"`, `"xmax"`, `"ymin"`,
  `"ymax"`, `"zmin"`, `"zmax"` for the box, or a pane of a curvilinear
  frame); or `list(axis = "y", at = 0)` for the plane where that axis
  takes a value in data units. A character vector, or a list mixing
  names and planes, draws the layer once per target.

- axes:

  `NULL` for a three-dimensional layer. For a two-dimensional ggplot2
  layer, the two 3D axes that receive its x and y, such as `c("x", "z")`
  to stand a histogram of x on a y wall. The layer is then drawn on the
  targets perpendicular to the remaining axis, and its x and y train the
  scales of those axes.

## Value

A ggplot2 position adjustment.

## Details

The replaced coordinate does not train the scales: a projected layer is
drawn on the expanded panel box of the other layers. Primitives on a
pane are painted with that pane: behind every data layer on a rear pane,
over them on a pane facing the camera, so a shadow is never hidden by
the pane it lies on. Planes at a data value are depth-sorted with the
data. Surface normals of a flattened layer point into the box.

The approach follows `position_on_face()` in ggcube, contour projections
in matplotlib (`zdir`, `offset`) and plot3D (`contour = list(side = )`).

## See also

[`geom_rug_3d()`](https://gillescolling.com/ggplot3d/reference/geom_rug_3d.md)
for marginal ticks on the walls.

## Examples

``` r
d <- data.frame(x = rnorm(60), y = rnorm(60), z = rnorm(60))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_point_3d(colour = "grey40", position = position_project_3d(c("floor", "back"))) +
  geom_point_3d(colour = "steelblue") + coord_3d()

g <- expand.grid(x = seq(-2, 2, length.out = 25), y = seq(-2, 2, length.out = 25))
g$z <- exp(-g$x^2 - g$y^2)
ggplot2::ggplot(g, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey85", alpha = 0.6) +
  geom_contour_3d(bins = 5, colour = "navy", position = position_project_3d("floor")) +
  geom_contour_filled_3d(bins = 5, position = position_project_3d("back")) +
  coord_3d()

p + geom_point_3d() +
  ggplot2::geom_density(ggplot2::aes(x = x, y = ggplot2::after_stat(scaled) * 2),
    inherit.aes = FALSE, fill = "grey60", alpha = 0.6,
    position = position_project_3d("back", axes = c("x", "z"))) +
  coord_3d()
```
