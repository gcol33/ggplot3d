# Annotations with a height

Adds a layer of fixed values like
[`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html),
with z positions as well. Every position vector has length one or a
common length.

## Usage

``` r
annotate_3d(
  geom,
  x = NULL,
  y = NULL,
  z = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  zmin = NULL,
  zmax = NULL,
  xend = NULL,
  yend = NULL,
  zend = NULL,
  ...,
  na.rm = FALSE
)
```

## Arguments

- geom:

  Name of a registered geom, such as `"point"` or `"bracket"`, or a
  ggplot2 geom prototype.

- x, y, z, xmin, xmax, ymin, ymax, zmin, zmax, xend, yend, zend:

  Positions.

- ...:

  Fixed aesthetics and drawing parameters passed to the 3D layer.

- na.rm:

  Whether to silently remove missing values.

## Value

A ggplot2 layer.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  annotate_3d("text", x = 2, y = 2, z = 3, label = "peak") +
  annotate_3d("segment", x = 1, y = 1, z = 0, xend = 1, yend = 1, zend = 2) +
  coord_3d()
```
