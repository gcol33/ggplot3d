# Stereo output for 3D panels

`stereo_3d()` configures
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
to draw every panel for two eyes, as a side-by-side stereo pair or as
one anaglyph to view through coloured glasses, in the manner of the
stereo displays of tourr (`display_stereo()`, `anaglyph()`) and ggthreed
(`stat_anaglyph()`).

## Usage

``` r
stereo_3d(
  mode = c("pair", "cross", "anaglyph"),
  separation = 3,
  colours = c("red", "cyan")
)
```

## Arguments

- mode:

  `"pair"`, `"cross"` or `"anaglyph"`.

- separation:

  Angle between the two eye directions in degrees, greater than 0 and
  less than 90.

- colours:

  Two filter colours, the left eye's first, as in the colour glasses
  used to view an anaglyph.

## Value

A stereo configuration for the `stereo` argument of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Details

The two eye cameras turn about the camera target by half of `separation`
each way, about the vertical axis of the image. For a level camera
without roll this is an azimuth offset of `separation / 2` per eye. Both
eyes look at the target, so the target lies in the page plane: geometry
in front of it appears in front of the page, geometry behind it behind
the page. In an orthographic view the two images differ only
horizontally. Both eyes share one magnification and one axis-label
reserve, so the images have the same scale.

`"pair"` places the left-eye image on the left, for parallel (wall-eyed)
free viewing and stereoscopes; `"cross"` places it on the right, for
cross-eyed viewing. Each panel then holds the two images side by side
and is twice as wide as high.

`"anaglyph"` draws both images into one panel. Each image is reduced to
its luminance and seen through its filter colour: the panel is the
product of the left image tinted toward the complement of the left
filter and the right image tinted toward the complement of the right
filter, so with red and cyan filters the red channel holds the left
image and the green and blue channels the right image. Paint seen
identically by both eyes, such as the panel background, keeps its grey
level. Colour and fill scales then carry only luminance;
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
warns when the plot has one. With the vector and hybrid backends the two
images are drawn as monochrome passes composited with a multiply blend
(grid group compositing, R 4.2 or later) when the graphics device
supports it, such as the ragg, Cairo, svg and pdf devices. Otherwise,
and always with the raster backend, both images are rendered offscreen
at the backend's `dpi` with ragg and composited per colour channel into
one image; on a white background this matches the blended result.

[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
shows the plot from the central camera, without stereo.

## See also

[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
[`facet_views()`](https://gillescolling.com/ggplot3d/reference/facet_views.md),
whose views each become a stereo pair.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 25), y = seq(-1, 1, length.out = 25))
d$z <- exp(-3 * (d$x^2 + d$y^2))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey85", colour = "grey30", linewidth = 0.2)
p + coord_3d(stereo = stereo_3d("pair"))
p + coord_3d(stereo = stereo_3d("cross", separation = 5))
p + coord_3d(stereo = stereo_3d("anaglyph", colours = c("red", "cyan")))
```
