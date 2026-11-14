# Images for textured faces

Wraps an image so that surface layers can drape it over their faces and
[`annotation_raster_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_raster_3d.md)
can lay it on a plane. Texture coordinates `u` (left to right) and `v`
(bottom to top) run from 0 to 1 across the image, as in OpenGL and rgl;
points outside the image take the face's fill, or nothing when the fill
is `NA`. The image is painted over the fill, so a transparent image
region shows the fill.

## Usage

``` r
texture_3d(image, interpolate = TRUE, extent = NULL)
```

## Arguments

- image:

  A `nativeRaster` (such as `png::readPNG(native = TRUE)`), a raster or
  matrix of colours, a numeric matrix of grey levels or a numeric array
  of grey, grey and alpha, RGB or RGBA channels in \[0, 1\] (such as
  [`png::readPNG()`](https://rdrr.io/pkg/png/man/readPNG.html)), or the
  path of a PNG file (needs png). Row one is the top of the image.

- interpolate:

  Whether to interpolate between pixels (bilinear and mipmapped
  sampling) or to show them as sharp squares, as
  [`grid::rasterGrob()`](https://rdrr.io/r/grid/grid.raster.html) does.

- extent:

  `NULL`, or four numbers `c(umin, umax, vmin, vmax)`: the range of the
  layer's texture coordinates that the image covers. A surface layer
  without explicit `u` and `v` uses its x and y positions (its
  parameters for
  [`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md),
  its texture coordinates for an rgl mesh), so an extent in data units
  places an orthophoto over a terrain. `NULL` stretches the image over
  the range of the layer's coordinates.

## Value

A `ggplot3d_texture` object.

## Details

The raster backend and the
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
viewer sample the image per sample, perspective-correctly, in linear
light: bilinearly where the image is magnified and from a mipmap
(Williams 1983) where it is minified. The vector backend draws unlit
images once per face triangle through an affine group transformation
clipped to the triangle (R \>= 4.2 groups, Murrell 2021). Under an
orthographic camera every triangle maps the image affinely, which is
exact; under perspective, triangles are split until the affine map lies
within half a device pixel of the projective one. Raster devices
resample the image with their own filter, in encoded sRGB. svglite
writes transformation matrices with two significant digits, which can
misplace the image by about 1% of its size; ragg, pdf(), cairo_pdf() and
svg() keep it in place. Lit faces use the shared texture sampler and
linear-light shading kernel to build colour-correct multiply and screen
layers in one clipped group per face run. The hybrid renderer handles
devices with limited compositing support. Text and labels are drawn over
the scene.
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
and glTF export
([`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md))
carry the image and the texture coordinates.

## References

Williams (1983). Pyramidal parametrics. SIGGRAPH Computer Graphics
17(3), 1-11.
[doi:10.1145/964967.801126](https://doi.org/10.1145/964967.801126)

Murrell (2021). Groups, compositing operators, and affine
transformations in R graphics. Technical report, University of Auckland.

## See also

[`annotation_raster_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_raster_3d.md),
[`geom_raster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_raster_3d.md)
and the `texture` argument of
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md)
and
[`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md).

## Examples

``` r
checker <- matrix(c("black", "white"), 8, 8)
checker[] <- ifelse((row(checker) + col(checker)) %% 2 == 0, "black", "white")
tex <- texture_3d(checker, interpolate = FALSE)
tex
d <- expand.grid(x = seq(-1, 1, length.out = 21), y = seq(-1, 1, length.out = 21))
d$z <- with(d, exp(-2 * (x^2 + y^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(texture = tex, shade = FALSE) +
  coord_3d()
```
