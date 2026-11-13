# Point, path, segment, text and polygon layers in three dimensions

Point, path, segment, text and polygon layers in three dimensions

## Usage

``` r
geom_point_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_path_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_segment_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_text_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_polygon_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

## Value

A ggplot2 layer emitting unprojected scene geometry.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(1, 2, 4),
                label = letters[1:3])
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_point_3d(size = 3) + coord_3d()
p + geom_path_3d() + coord_3d()
p + geom_segment_3d(ggplot2::aes(xend = x, yend = y, zend = 0)) + coord_3d()
p + geom_text_3d(ggplot2::aes(label = label)) + coord_3d()
p + geom_polygon_3d(fill = "steelblue", alpha = 0.5) + coord_3d()
```
