# Camera-aligned orientation guides

A triad shows the positive model x, y and z directions in each panel,
including panels drawn without axes. Its corner and physical size stay
fixed as the camera moves, as in the orientation widget in ParaView. Add
it with `ggplot2::guides(orientation = guide_triad_3d())`.
`annotation_north_3d()` adds a single direction arrow through the same
guide system. Its `north` vector specifies north in model coordinates.

## Usage

``` r
guide_triad_3d(
  position = "bottomleft",
  size = grid::unit(16, "mm"),
  labels = c("x", "y", "z"),
  colour = c("#B33B33", "#278048", "#3166AD"),
  linewidth = 0.5,
  theme = NULL
)

annotation_north_3d(
  north = c(0, 1, 0),
  label = "N",
  position = "topright",
  size = grid::unit(16, "mm"),
  colour = "black",
  linewidth = 0.5,
  theme = NULL
)
```

## Arguments

- position:

  One of `"bottomleft"`, `"bottomright"`, `"topleft"`, `"topright"`.

- size:

  Width and height of the guide as a physical grid unit.

- labels:

  Three axis labels.

- colour:

  Three axis colours, or one colour for every axis.

- linewidth:

  Arrow width in millimetres.

- theme:

  Local theme overrides for the text (`axis.text`).

- north:

  A finite, nonzero numeric direction vector of length three.

- label:

  Label of the north arrow.

## Value

`guide_triad_3d()` returns a guide; `annotation_north_3d()` returns a
ggplot2 guides specification that can be added to a plot.

## References

ParaView, `vtkPVAxesWidget`; lattice, `panel.cloud` direction arrows.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  coord_3d(axes = character())
p + ggplot2::guides(orientation = guide_triad_3d())
p + annotation_north_3d(north = c(0, 1, 0))
```
