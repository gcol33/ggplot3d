# Reference lines through the panel box

`geom_refline_3d()` draws lines parallel to an axis: fix two of
`xintercept`, `yintercept` and `zintercept` and the line runs through
the whole box along the third axis, the three-dimensional counterpart of
[`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
and
[`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).
`geom_abline_3d()` draws lines in any direction through a point, the
counterpart of
[`ggplot2::geom_abline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

## Usage

``` r
geom_refline_3d(
  mapping = NULL,
  data = NULL,
  ...,
  xintercept,
  yintercept,
  zintercept,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)

geom_abline_3d(
  mapping = NULL,
  data = NULL,
  ...,
  point = c(0, 0, 0),
  direction = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping, data, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments. `...` takes fixed aesthetics such as
  `colour`, `linewidth`, `linetype` and `alpha`, and the 3D drawing
  parameters such as `hidden` and `occlusion`.

- xintercept, yintercept, zintercept:

  Positions of the held axes. Give exactly two; vectors of length one or
  a common length draw several lines.

- point:

  A point each line passes through: a numeric vector `c(x, y, z)`, or a
  three-column matrix with one row per line.

- direction:

  Directions of the lines: a numeric vector `c(dx, dy, dz)` or a
  three-column matrix with one row per line. When `NULL`, the directions
  come from the `dx`, `dy` and `dz` aesthetics.

## Value

A ggplot2 layer.

## Details

Intercepts given as arguments draw one line per value and stay out of
the legend; mapped intercepts draw one line per row. As in ggplot2,
intercepts are positions and expand the axis ranges to include them.

A line from `geom_abline_3d()` passes through `point` along `direction`.
Both are in the plot's transformed scale space, so a direction of
`c(1, 0, 2)` rises two z units per x unit. The point and direction do
not expand the axis ranges: each line is cut to the panel box when the
panel is drawn, and a line that misses the box is skipped with a
warning. Directions are given as arguments (one line per row of a
three-column matrix) or mapped to `dx`, `dy` and `dz`; mapped lines
share one point.

Both lines are depth-sorted with the rest of the scene. In a curvilinear
frame such as
[`coord_cylindrical_3d()`](https://gillescolling.com/ggplot3d/reference/coord_cylindrical_3d.md)
they bend with the frame, as a horizontal line becomes a circle in
[`ggplot2::coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html).

## Aesthetics

`geom_refline_3d()` understands `xintercept`, `yintercept`,
`zintercept`, `colour`, `linewidth`, `linetype` and `alpha`;
`geom_abline_3d()` understands `dx`, `dy`, `dz`, `colour`, `linewidth`,
`linetype` and `alpha`.

## Examples

``` r
d <- data.frame(x = rnorm(60), y = rnorm(60), z = rnorm(60))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()

# A vertical line through the origin and a line along x at y = 1, z = 0
p + geom_refline_3d(xintercept = 0, yintercept = 0, colour = "red") +
  geom_refline_3d(yintercept = 1, zintercept = 0, linetype = 2) +
  coord_3d()

# The diagonal through the centre of the data, and the first principal axis
axis1 <- prcomp(d)$rotation[, 1]
p + geom_abline_3d(point = colMeans(d), direction = rbind(c(1, 1, 1), axis1),
  colour = c("grey50", "red")) +
  coord_3d()
```
