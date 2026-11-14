# Describe a three-dimensional plot for a document

Describes the layers, mapped variables and initial camera. An explicit
`labs(alt = ...)` takes precedence. Review the description when the plot
communicates a finding that needs words beyond its mapped variables.

## Usage

``` r
alt_text_3d(plot)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Value

One character string.

## Details

In knitr and Quarto documents, `view3d = TRUE` in a chunk selects
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
for HTML output. PDF and Word receive a static plot drawn with the
vector scene backend. The chunk's `fig.alt` overrides the generated
description. Widget dimensions follow `fig.width` and `fig.height` in
inches at 96 pixels per inch.

The chunk option `raster3d` keeps a static plot of dense geometry small:
`TRUE` draws its vector-backend panels with
`backend_3d("hybrid", rasterise = "surfaces")`, which draws the faces of
every depth-tested or rear-pane layer into one image while paths,
points, axes, panes, grid lines and text stay vector graphics. A number
is a file size in bytes: the plot is drawn as vector graphics and, while
the file exceeds that size, again with `rasterise = "surfaces"` and then
with `rasterise = "all"`, which also draws paths and points into the
image; the smallest file is kept. The image has the chunk's `dpi` pixels
per inch, which knitr multiplies by `fig.retina` for HTML output: the
density of a raster figure from the same chunk. Panels with another
backend are left as they are. The same applies to patchwork compositions
that contain ggplot3d plots.

For a raster scene saved with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
set both `backend_3d("raster", dpi = 300)` and `ggsave(..., dpi = 300)`
for a 300 dpi image. The backend dpi controls scene sampling at its
physical panel size; the device dpi controls the saved image's pixel
dimensions.

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
  geom_point_3d() + coord_3d(azimuth = 40, elevation = 25)
alt_text_3d(p)
alt_text_3d(p + ggplot2::labs(alt = "Fuel economy, weight and horsepower."))
```
