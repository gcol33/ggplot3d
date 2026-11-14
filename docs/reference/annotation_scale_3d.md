# A scale bar lying in a coordinate plane

Draws a bar of the given length in original data units, with in-plane
end ticks and a label at the same depth. Perspective foreshortens it
together with that plane. It measures distance locally at its own
position. The annotation uses the trained scales and leaves their ranges
unchanged.

## Usage

``` r
annotation_scale_3d(
  axis = "x",
  length = 1,
  unit = "",
  plane = "floor",
  at = NULL,
  label = NULL,
  ...
)
```

## Arguments

- axis:

  Axis along which the bar measures distance: `"x"`, `"y"`, `"z"`.

- length:

  Positive bar length in original data units.

- unit:

  Text appended to the length.

- plane:

  `"floor"`, a box pane name, `"xy"`, `"xz"`, `"yz"`, or a
  `list(axis = , at = )` plane as in
  [`position_project_3d()`](https://gillescolling.com/ggplot3d/reference/position_project_3d.md).

- at:

  Optional named numeric position in original data units. Missing
  coordinates start one tenth of the way across the trained panel range;
  the plane fixes the coordinate perpendicular to it.

- label:

  Label text, defaulting to the length and unit.

- ...:

  Fixed appearance such as `colour`, `linewidth` and text `size`.

## Value

A ggplot2 layer.

## Examples

``` r
d <- data.frame(x = c(0, 1000), y = c(0, 1000), z = c(0, 100))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  annotation_scale_3d(length = 100, unit = "m") +
  coord_3d(projection = "perspective")
```
