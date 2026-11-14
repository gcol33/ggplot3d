# Rug ticks on the walls of the panel box

Marks the values of one position on a pane of the panel box with short
ticks, the three-dimensional counterpart of
[`ggplot2::geom_rug()`](https://ggplot2.tidyverse.org/reference/geom_rug.html).

## Usage

``` r
geom_rug_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  sides = c("floor:x", "floor:y"),
  length = 0.03,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- sides:

  Character vector of `"pane:axis"` entries. The pane is `"floor"`,
  `"back"` or a pane name as in
  [`position_project_3d()`](https://gillescolling.com/ggplot3d/reference/position_project_3d.md);
  the axis is the position whose values are marked. `"floor:x"` ticks
  the x values on the floor, `"back:z"` the z values on the back walls.
  An optional third part names the pane edge the ticks start from, such
  as `"floor:x:ymax"`.

- length:

  Tick length as a fraction of the pane's extent.

## Value

A ggplot2 layer.

## Details

Ticks lie in the pane, perpendicular to the marked axis. Without an edge
they start at the pane edge on the camera side, where the axis labels
usually sit. Like
[`position_project_3d()`](https://gillescolling.com/ggplot3d/reference/position_project_3d.md),
ticks are painted with their pane, and an axis the layer does not map
draws no ticks.

## Examples

``` r
d <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d() +
  geom_rug_3d(sides = c("floor:x", "floor:y", "back:z"), alpha = 0.5) +
  coord_3d()
```
