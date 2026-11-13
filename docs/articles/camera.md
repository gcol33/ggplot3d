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
