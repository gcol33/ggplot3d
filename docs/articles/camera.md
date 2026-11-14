# Camera and framing

Camera controls choose the direction from which a scene is viewed.
Framing controls the bounds used to fit that view on the page.
Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).

``` r

d <- expand.grid(x = 1:3, y = 1:3)
d$z <- c(1, 2, 4, 3, 5, 2, 4, 2, 3)
p <- ggplot(d, aes(x, y, z = z)) +
  geom_col_3d(fill = PAL["blue"], width = 0.65, depth = 0.65)
```

## Angles

Azimuth rotates the camera around the vertical axis. Elevation raises
the view above the horizontal plane. Roll rotates the image around the
viewing direction. All three angles use degrees.

``` r

p + coord_3d(azimuth = -30, elevation = 35, roll = 10)
```

![Columns seen at azimuth minus thirty degrees, elevation thirty-five
degrees and a ten-degree roll.](camera_files/figure-html/angles-1.svg)

## Projection

Orthographic projection keeps the apparent size of an object independent
of its distance from the camera. Perspective projection makes nearer
objects appear larger. The field of view is set in degrees with `fov`;
`distance` controls the camera’s distance.

``` r

p + coord_3d(projection = "perspective", azimuth = -60,
             elevation = 25, fov = 35)
```

![Columns viewed with perspective, making nearer columns appear
larger.](camera_files/figure-html/perspective-1.svg)

The `target` argument gives the point the camera looks at as fractions
along the expanded x, y and z ranges. Its default is the centre of that
box. Increase `zoom` to enlarge the view within the selected frame.

## Framing a rotating view

The default frame fits the current view. An azimuth frame reserves
bounds for rotation around the vertical axis at the current elevation. A
sphere frame reserves bounds for arbitrary rotation. Use a common frame
when a sequence of views should retain its scale.

``` r

p + coord_3d(azimuth = -30, elevation = 30, frame = "azimuth")
```

![Columns inside a frame reserved for rotation around the vertical
axis.](camera_files/figure-html/frame-1.svg)

## Aspect and the box

The aspect gives the three side lengths of the model box. The default
makes the three sides equal. Set `aspect = "data"` to use the
transformed data ranges. Panes are the faces drawn behind the data; axes
and box edges can be selected separately.

``` r

p + coord_3d(aspect = c(1, 1, 2), panes = "floor", box = "all")
```

![Columns inside a box twice as tall as its horizontal sides, with a
floor pane.](camera_files/figure-html/box-1.svg)

Lighting changes the colours used to draw surface faces. Its direction
can be expressed in camera coordinates or data coordinates. Use camera
space to keep lighting tied to the view.

``` r

p + coord_3d(light = light_3d(direction = c(1, 1, 2), space = "camera"))
```

![Blue columns lit from the upper right of the
view.](camera_files/figure-html/light-1.svg)

## Stereo pairs

Stereo output renders the scene from two eye directions. Each eye turns
by half the separation about the image’s vertical axis while looking at
the same target. The target lies in the page plane, with nearer geometry
appearing in front of it. Both eyes share the same magnification.

``` r

p + coord_3d(axes = character(), panes = character(), box = "none",
  stereo = stereo_3d("pair", separation = 5))
```

![Left-eye and right-eye views of the columns placed side by side for
parallel stereo viewing.](camera_files/figure-html/stereo-pair-1.svg)

Pair mode places the left-eye image on the left for parallel viewing or
a stereoscope. Cross mode reverses their order for cross-eyed viewing.
Each facet gets its own eye pair. The separation is an angle in degrees.

``` r

p + coord_3d(axes = character(), panes = character(), box = "none",
  stereo = stereo_3d("cross", separation = 5))
```

![The column scene with the right-eye image on the left and the left-eye
image on the right for cross-eyed
viewing.](camera_files/figure-html/stereo-cross-1.svg)

## Anaglyphs

An anaglyph combines the two views into colour channels for coloured
glasses. The default filters are red for the left eye and cyan for the
right. Each eye image is reduced to luminance, so mapped colour scales
communicate brightness in this mode. A plot with a colour or fill scale
produces a warning.

``` r

p + coord_3d(stereo = stereo_3d("anaglyph", separation = 5),
  backend = backend_3d("raster", dpi = 80, antialias = 1))
```

![Red-cyan anaglyph of the column scene for viewing through coloured
glasses.](camera_files/figure-html/stereo-anaglyph-1.svg)

Vector and hybrid output use device multiply compositing when available.
Raster output composites the two eye images per channel; devices without
multiply support also use that raster composite and require ragg.
[Interactive
viewing](https://gillescolling.com/ggplot3d/articles/viewer.md) uses the
central camera in a browser. Intrinsic stereo needs separately
configured observers and orientations for each eye in [Inside the
Thurston
geometries](https://gillescolling.com/ggplot3d/articles/thurston-geometries.md).
