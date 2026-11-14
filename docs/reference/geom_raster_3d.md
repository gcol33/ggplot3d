# Rasters on planes

Draws values on a regular grid as one image lying on a plane of the
panel, like
[`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html):
each cell is one pixel of a texture (see
[`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md)),
so a raster of any size is a single face of the scene. The two positions
of `plane` locate the cells; the remaining axis takes `at`, or the
mapped position on it, and every distinct position on that axis gives
its own image, such as one slice of a volume per level.

## Usage

``` r
geom_raster_3d(
  mapping = NULL,
  data = NULL,
  stat = StatRaster3D,
  position = "identity",
  ...,
  plane = "xy",
  at = NULL,
  interpolate = FALSE,
  hjust = 0.5,
  vjust = 0.5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- plane:

  Two distinct axis letters: the axes of the cell positions, such as
  `"xy"`, `"xz"` or `"yz"`.

- at:

  Position on the remaining axis, overriding a mapped one; `NULL` uses
  the mapped position, or the lower box side when it is not mapped.

- interpolate:

  Whether to interpolate between cells. The default shows them as sharp
  squares.

- hjust, vjust:

  Position of each cell's centre within the cell, as in
  [`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

## Value

A ggplot2 layer.

## See also

[`annotation_raster_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_raster_3d.md)
for an image,
[`facet_slices_3d()`](https://gillescolling.com/ggplot3d/reference/facet_slices_3d.md).

## Examples

``` r
d <- expand.grid(x = 1:30, y = 1:20)
d$value <- with(d, sin(x / 4) * cos(y / 3))
ggplot2::ggplot(d, ggplot2::aes(x, y, fill = value)) +
  geom_raster_3d() +
  coord_3d()
slices <- expand.grid(x = 1:25, y = 1:25, z = c(1, 2, 3))
slices$value <- with(slices, exp(-((x - 13)^2 + (y - 13)^2) / (20 * z)))
ggplot2::ggplot(slices, ggplot2::aes(x, y, z = z, fill = value)) +
  geom_raster_3d(alpha = 0.8) +
  coord_3d()
```
