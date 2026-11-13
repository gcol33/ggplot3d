# Contours following a triangulated surface or a horizontal plane

Contours following a triangulated surface or a horizontal plane

## Usage

``` r
geom_contour_3d(
  mapping = NULL,
  data = NULL,
  stat = StatContour3D,
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  plane = c("surface", "floor"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_contour_filled_3d(
  mapping = NULL,
  data = NULL,
  stat = StatContourFilled3D,
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  plane = c("surface", "floor"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- bins, binwidth, breaks:

  Level controls in transformed z units. Explicit breaks take
  precedence.

- plane:

  Either surface, floor, or a numeric z in original units.

## Value

A ggplot2 layer.

## Details

Isoband operates on the same grid triangles used by geom_surface_3d().
Default levels are interior pretty(range, n = 10) values. Surface bands
are lifted by barycentric interpolation within each triangle. Floor
contours use the lower expanded panel boundary. The source height range
trains the z scale for either projection.

## Examples

``` r
d <- expand.grid(x = seq(-2, 2, length.out = 12), y = seq(-2, 2, length.out = 12))
d$z <- exp(-d$x^2 - d$y^2)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d() + geom_contour_3d(bins = 4, colour = "navy") + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_contour_filled_3d(bins = 4, plane = "floor") + coord_3d()
```
