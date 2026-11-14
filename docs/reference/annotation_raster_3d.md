# An image on a plane of the panel

Lays an image on a plane of the 3D panel, such as a map on the floor or
a photograph on a wall. The image is one textured face of the scene: it
is foreshortened with the plane and hidden by what lies in front of it,
and the raster backend and the
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
viewer sample it perspective correctly (see
[`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md)).
Unlike
[`ggplot2::annotation_raster()`](https://ggplot2.tidyverse.org/reference/annotation_raster.html),
which coord_3d() also draws this way on the floor, the image can stand
on any axis plane.

## Usage

``` r
annotation_raster_3d(
  raster,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  plane = "xy",
  at = -Inf,
  interpolate = TRUE,
  alpha = 1
)
```

## Arguments

- raster:

  An image accepted by
  [`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md),
  or a texture.

- xmin, xmax, ymin, ymax:

  Image extent along the first and second axis of `plane`.

- plane:

  Two distinct axis letters, such as `"xy"`, `"xz"` or `"yz"`, or
  `"floor"` for `"xy"`.

- at:

  Position on the remaining axis; `-Inf` and `Inf` place the image on
  the lower and upper box side (0 when no other layer trains that axis).

- interpolate:

  Whether to interpolate between pixels, see
  [`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md).

- alpha:

  Opacity of the image.

## Value

A ggplot2 layer.

## Details

The image's horizontal direction runs along the first axis of `plane`
from `xmin` to `xmax`, its vertical direction along the second axis from
`ymin` to `ymax`; a reversed pair mirrors it. Infinite positions reach
the sides of the panel box. The remaining axis is fixed at `at`.

## See also

[`geom_raster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_raster_3d.md)
for data on a regular grid,
[`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md).

## Examples

``` r
logo <- matrix(grDevices::hcl.colors(12, "Zissou 1"), 3, 4)
d <- data.frame(x = runif(30), y = runif(30), z = runif(30))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  annotation_raster_3d(logo, 0, 1, 0, 1, plane = "xz", at = Inf, interpolate = FALSE) +
  annotation_raster_3d(logo, 0, 1, 0, 1, plane = "floor") +
  geom_point_3d() +
  coord_3d()
```
