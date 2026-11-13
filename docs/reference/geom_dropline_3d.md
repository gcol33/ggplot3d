# Drop segments from points to a box face

Drop segments from points to a box face

## Usage

``` r
geom_dropline_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  face = "zmin",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- face:

  Box face: zmin (floor), zmax, xmin, xmax, ymin or ymax.

## Value

A ggplot2 layer.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_dropline_3d() + geom_point_3d() + coord_3d()
```
